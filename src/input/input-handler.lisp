(in-package :recurse.vert)

(defclass input-handler (game-object)
  ((input-command-map
    :initform nil
    :type hash-table
    :documentation "hash-table of device-name -> input-plist. Maps inputs to commands.")
   (command-action-map
    :initform nil
    :type hash-table
    :documentation "hash-table of command -> (while-active-lambda on-deactivate-lambda). Maps commands to actions.")
   (active-input-device
    :initarg :active-input-device
    :initform nil
    :accessor active-input-device
    :documentation "The DEVICE-ID of the input device the input handler is hooked up to.")
   (last-input-device
    :initform nil
    :reader input-handler-get-input-device
    :documentation "The last input-device with an activated input that this handler is hooked up to."))
  (:documentation "Map raw inputs into user-defined commands."))

(export '(input-handler-get-input-device))

(let ((empty-mappings (make-hash-table)))
  (defgeneric %default-input-command-map (input-handler)
    (:method (input-handler)
      empty-mappings))
  (defgeneric %default-command-action-map (input-handler)
    (:method (input-handler)
      empty-mappings)))

(defun %%handle-input (input-handler input-map command-map input-device input input-type)
  (let* ((input-plist (gethash (input-device-type input-device) input-map))
         (command (and input-plist (getf input-plist input)))
         (command-mapping (gethash command command-map))
         (action (and command-mapping
                      (ecase input-type
                        (:activated (elt command-mapping 0))
                        (:active (elt command-mapping 1))
                        (:deactivated (elt command-mapping 2))))))
    (when action (funcall action input-handler (device-id input-device)))))

(defun %handle-activated-input (input-handler input-device input)
  (%%handle-input input-handler
                  (or (slot-value input-handler 'input-command-map)
                      (%default-input-command-map input-handler))
                  (or (slot-value input-handler 'command-action-map)
                      (%default-command-action-map input-handler))
                  input-device
                  input
                  :activated))

(defun %handle-active-input (input-handler input-device input)
  (%%handle-input input-handler
                  (or (slot-value input-handler 'input-command-map)
                      (%default-input-command-map input-handler))
                  (or (slot-value input-handler 'command-action-map)
                      (%default-command-action-map input-handler))
                  input-device
                  input
                  :active))

(defun %handle-deactivated-input (input-handler input-device input)
  (%%handle-input input-handler
                  (or (slot-value input-handler 'input-command-map)
                      (%default-input-command-map input-handler))
                  (or (slot-value input-handler 'command-action-map)
                      (%default-command-action-map input-handler))
                  input-device
                  input
                  :deactivated))

(let ((vec (make-array 8
                       :element-type 'keyword
                       :adjustable t
                       :fill-pointer 0
                       :initial-element :no-input)))
  (defun %remove-duplicate-commands (input-handler input-device input-vector)
    "Return a new vector which is identical to INPUT-VECTOR except all duplicate input commands which cause the same action will be removed (as defined by INPUT-HANDLER).
For example, if the handler allows for keyboard :scancode-right and :scancode-d to move the player right, remove the second duplicate input when both are pressed.
The returned vector will be re-used by subsequent calls so callers must not hold a reference to it."
    (setf (fill-pointer vec) 0)
    (let* ((input-map (or (slot-value input-handler 'input-command-map)
                          (%default-input-command-map input-handler)))
           (input-plist (gethash (input-device-type input-device) input-map)))
      (loop :for i :from 0 :below (length input-vector) :do
           (let* ((input (elt input-vector i))
                  (command (and input-plist (getf input-plist input)))
                  (duplicate-command-p
                   (when command (loop :for j :from (- i 1) :downto 0 :do
                                      (when (eq command (getf input-plist (elt input-vector j)))
                                        (return t))))))
             (cond ((null command)
                    (log:debug "~A : ignoring input with no command mapping: ~A"
                               input-handler
                               (elt input-vector i)))
                   (duplicate-command-p
                    (log:debug "~A : ignoring input with no duplicate command: ~A -> ~A"
                               input-handler input command))
                   (t (vector-push-extend input vec))))))
    vec))

(defmethod update ((input-handler input-handler))
  (prog1 (call-next-method input-handler)
    (when (active-input-device input-handler)
      ;; TODO: instead of `scene-input` make a generic getter (separate from the concept of scenes)
      (loop :for input-device :across (scene-input *scene*) :do
           (when (or (eql *all-input-id* (active-input-device input-handler))
                     (eql (active-input-device input-handler) (device-id input-device)))
             (let ((device-sending-input-p nil))
               (loop :for input :across (%remove-duplicate-commands
                                         input-handler
                                         input-device
                                         (get-activated-inputs input-device)) :do
                    (setf device-sending-input-p t)
                    (with-slots (last-input-device) input-handler
                      (unless (eq input-device last-input-device)
                        (setf last-input-device input-device)))
                    (%handle-activated-input input-handler input-device input))
               (loop :for input :across (%remove-duplicate-commands
                                         input-handler
                                         input-device
                                         (get-active-inputs input-device)) :do
                    (setf device-sending-input-p t)
                    (%handle-active-input input-handler input-device input))
               (loop :for input :across (%remove-duplicate-commands
                                         input-handler
                                         input-device
                                         (get-deactivated-inputs input-device)) :do
                    (setf device-sending-input-p t)
                    (%handle-deactivated-input input-handler input-device input))
               (when device-sending-input-p
                 ;; process input from the first configured device that is sending input
                 (return))))))))

(defmacro set-default-input-command-map (classname &rest input-mappings)
  "Set the default input-command map for CLASSNAME."
  `(let ((input-command-map (%make-input-map ',input-mappings)))
     (defmethod %default-input-command-map ((input-handler ,classname))
       input-command-map)))

(defmacro override-input-command-map (input-handler &rest input-mappings)
  `(setf (slot-value ,input-handler 'input-command-map)
         (%make-input-map ',input-mappings)))

(defun %make-input-map (device-mappings)
  "Convert a nested lambda list into an input map hash table"
  (when (> (length device-mappings) 0)
    (loop with input-map = (make-hash-table :size (length device-mappings)  :test #'eq)
       for device-mapping in device-mappings do
         (assert (keywordp (first device-mapping)))
         (assert (every (lambda (mapping)
                          (and (listp mapping)
                               (= 2 (length mapping))
                               (keywordp (first mapping))
                               (keywordp (second mapping))))
                        (rest device-mapping)))
         (setf (gethash (first device-mapping) input-map) (alexandria:flatten (rest device-mapping)))
       finally (return input-map))))

;; TODO: Document DEVICE-ID
(defmacro set-default-command-action-map (classname &rest command-mappings)
  "Set the default command -> action mappings for the class CLASSNAME."
  `(let ((command-action-map (%make-command-map ,classname ,@command-mappings)))
     (defmethod %default-command-action-map ((input-handler ,classname))
       command-action-map)))

;; TODO: Document DEVICE-ID
(defmacro override-command-action-map (input-handler &rest command-mappings)
  `(setf (slot-value ,input-handler 'command-action-map) (%make-command-map ,input-handler ,@command-mappings)))

(defmacro %make-command-map (classname &rest command-mappings)
  (labels ((action-type (action)
             (ecase (intern (symbol-name (first action)) (symbol-package 'while-active))
               (on-activate)
               (while-active)
               (on-deactivate))
             (intern (symbol-name (first action)) (symbol-package 'while-active)))
           (find-action (action-name &rest actions)
             (loop :for action :in actions :do
                  (when (and action
                             (eq action-name (action-type action)))
                    (return action))))
           (make-action-lambda (action)
             (when action
               `(lambda (,classname device-id)
                  (declare (ignorable ,classname device-id))
                  ,@(rest action))))
           (make-action-lambdas (action1 &optional action2 action3)
             (when action2
               (assert (not (eq (action-type action1) (action-type action2)))))
             (when action3
               (assert (not (eq (action-type action1) (action-type action3))))
               (assert (not (eq (action-type action2) (action-type action3)))))
             (loop :with action-lambdas = (list)
                :for action :in '(on-activate while-active on-deactivate) :do
                  (if (find-action action action1 action2 action3)
                    (push (make-action-lambda (find-action action action1 action2 action3))
                          action-lambdas)
                    (push nil action-lambdas))
                :finally (return `(make-array 3 :initial-contents (list ,@(nreverse action-lambdas)))))))
    `(let ((command-hash-table (make-hash-table :size ,(length command-mappings) :test #'eq)))
       ,@(loop
            for command-mapping in command-mappings do
              (assert (or (= 2 (length command-mapping))
                          (= 3 (length command-mapping))
                          (= 4 (length command-mapping))))
              (assert (keywordp (first command-mapping)))
            collect
              `(setf (gethash ,(first command-mapping) command-hash-table)
                     ,(make-action-lambdas (second command-mapping)
                                           (third command-mapping)
                                           (fourth command-mapping))))
       command-hash-table)))
