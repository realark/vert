(in-package :recurse.vert)

(defstruct %state
  "A single state with a name and actions."
  (name (error ":name required") :type keyword)
  (on-activate nil :type (or null function))
  (while-active nil :type (or null function))
  (on-deactivate nil :type (or null function)))

(defstruct %state-machine
  "A set of states and a pointer to the active state."
  (name (error ":name required") :type keyword)
  (active 0 :type (integer 0 *))
  (states (error ":states required") :type vector))

@export
(defclass stateful (game-object)
  ((state-machines :initform (make-hash-table)
                   :documentation ":state-machine-name -> %state-machine")
   (pending-state-changes :initform (make-hash-table)
                          :documentation "Upcoming state changes. Will transition on next UPDATE-USER."))
  (:documentation "A game object with various states. Appropriate state actions will be invoked after UPDATE-USER on the game object."))

@export
(defun add-state-machine (stateful-game-object state-machine)
  "Add STATE-MACHINE to STATEFUL's states."
  (declare (stateful stateful-game-object)
           (%state-machine state-machine))
  (with-slots (state-machines) stateful-game-object
    (if (gethash (%state-machine-name state-machine) state-machines)
        (error "~A state-machine already present in ~A"
               (%state-machine-name state-machine)
               stateful-game-object)
        (setf (gethash (%state-machine-name state-machine) state-machines) state-machine))))

@export
(defun current-state-for (stateful-game-object state-machine-name)
  "Return the current state (keyword) for STATEFUL-GAME-OBJECT's state-machine with the name of STATE-MACHINE-NAME."
  (declare (stateful stateful-game-object)
           (keyword state-machine-name))
  (with-slots (state-machines) stateful-game-object
    (let ((state-machine (gethash state-machine-name state-machines)))
      (when state-machine
        (%state-name (elt (%state-machine-states state-machine) (%state-machine-active state-machine)))))))

@export
(defun change-state (stateful-game-object world-context state-machine-name new-state-name)
  "Queue up a state change for STATEFUL-GAME-OBJECT to run on next UPDATE-USER."
  (declare (stateful stateful-game-object)
           (keyword state-machine-name new-state-name))
  (with-slots (state-machines) stateful-game-object
    (let* ((state-machine (gethash state-machine-name state-machines))
           (current-state (when state-machine (elt (%state-machine-states state-machine) (%state-machine-active state-machine)))))
      (unless state-machine
        (error "~A state-machine not present in ~A"
               state-machine-name
               stateful-game-object))
      (unless (eq (%state-name current-state) new-state-name)
        (loop :for i :from 0
           :for state :across (%state-machine-states state-machine) :do
             (when (eq new-state-name (%state-name state))
               (let ((old-state (elt (%state-machine-states state-machine) (%state-machine-active state-machine)))
                     (new-state (elt (%state-machine-states state-machine) i)))
                 (when (%state-on-deactivate old-state)
                   (funcall (%state-on-deactivate old-state)
                            :game-object stateful-game-object :world-context world-context :next-state new-state))
                 (setf (%state-machine-active state-machine) i)
                 (when (%state-on-activate new-state)
                   (funcall (%state-on-activate new-state)
                            :game-object stateful-game-object :world-context world-context :previous-state old-state)))
               (return))
           :finally (error "State ~A not found in state machine ~A" new-state-name state-machine-name))))))

(defmethod update-user :after ((stateful stateful) delta-t-ms world-context)
  (with-slots (state-machines) stateful ; invoke active state actions
    (loop :for state-machine :being :the hash-values :of state-machines :do
         (let ((while-active-fn (%state-while-active  (elt (%state-machine-states state-machine) (%state-machine-active state-machine)))))
           (when while-active-fn
             (funcall while-active-fn
                      :game-object stateful :delta-t-ms delta-t-ms :world-context world-context))))))

;; TODO: optimze to def-state-machine to share underlying state structures (which are stateless themselves and should be reused.)
@export
(defmacro make-state-machine ((&key name initial-state) &body states-and-actions)
  (unless (> (length states-and-actions) 0) (error "At least one state must be defined."))

  (let ((valid-state-actions '(:on-activate :while-active :on-deactivate))
        (valid-state-action-args '(:game-object :delta-t-ms :world-context :previous-state :next-state))
        (initial-state-index 0))
    (labels ((create-state-action-arg-list (args)
               `(&key
                 ,@(loop :for (arg-keyword arg-binding) :on args :by #'cddr
                      :collect (list (list arg-keyword arg-binding))
                      :do (unless (find arg-keyword valid-state-action-args)
                            (error "illegal state arg ~A. Expected one of ~A"
                                   arg-keyword
                                   valid-state-action-args)))
                 &allow-other-keys))
             (make-state (state-name state-actions)
               `(make-%state :name ,state-name
                             ,@(loop :for action :in state-actions
                                  :collect (first action)
                                  :collect
                                    `(lambda ,(create-state-action-arg-list (second action))
                                       ,@(rest (rest action)))
                                  :do (unless (find (first action) valid-state-actions)
                                        (error "illegal state action ~A. Expected one of ~A"
                                               (first action)
                                               valid-state-actions))))))
      `(make-%state-machine
        :name ,name
        :states (make-array ,(length states-and-actions)
                            :initial-contents
                            (list ,@(loop
                                       :for i :from 0
                                       :for state-and-actions :in states-and-actions
                                       :collect (make-state (first state-and-actions)
                                                            (rest state-and-actions))
                                       :do (when (and initial-state (eq initial-state (first state-and-actions)))
                                             (setf initial-state-index i)))))
        :active ,initial-state-index))))
