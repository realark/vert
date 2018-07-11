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
    :documentation "The DEVICE-ID of the input device the input handler is hooked up to."))
  (:documentation "Map raw inputs into user-defined commands."))

(defgeneric %default-input-command-map (input-handler))
(defgeneric %default-command-action-map (input-handler))

(defun %%handle-input (input-handler input-map command-map input-device input input-type)
  (let* ((input-plist (gethash (input-name input-device) input-map))
         (command (and input-plist (getf input-plist input)))
         (command-mapping (gethash command command-map))
         (action (and command-mapping
                      (ecase input-type
                        (:active (car command-mapping))
                        (:deactivated (cdr command-mapping))))))
    (when action (funcall action input-handler (device-id input-device)))))

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

(defmethod update-input ((input-handler input-handler) delta-t-ms input-context)
  (when (active-input-device input-handler)
    (loop for input-device across (scene-input input-context) do
         (when (or (= *all-input-id* (active-input-device input-handler))
                   (= (active-input-device input-handler) (device-id input-device)))
           (loop for input across (get-active-inputs input-device) do
                (%handle-active-input input-handler input-device input))
           (loop for input across (get-deactivated-inputs input-device) do
                (%handle-deactivated-input input-handler input-device input))))))

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
    (loop with input-map = (make-hash-table :size (length device-mappings)  :test #'equalp)
       for device-mapping in device-mappings do
         (assert (stringp (first device-mapping)))
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
               (while-active)
               (on-deactivate))
             (intern (symbol-name (first action)) (symbol-package 'while-active)))
           (make-action-lambda (action)
             (when action
               `(lambda (,classname device-id)
                  (declare (ignorable ,classname device-id))
                  ,@(rest action))))
           (make-action-cons (action1 &optional action2)
             (when action2
               (assert (not (eq (action-type action1) (action-type action2)))))
             (if (eq 'while-active (action-type action1))
                 `(cons ,(make-action-lambda action1) ,(make-action-lambda action2))
                 `(cons ,(make-action-lambda action2) ,(make-action-lambda action1)))))
    `(let ((command-hash-table (make-hash-table :size ,(length command-mappings))))
       ,@(loop
            for command-mapping in command-mappings do
              (assert (or (= 2 (length command-mapping))
                          (= 3 (length command-mapping))))
              (assert (keywordp (first command-mapping)))
            collect
              `(setf (gethash ,(first command-mapping) command-hash-table)
                     ,(make-action-cons (second command-mapping) (third command-mapping))))
       command-hash-table)))
