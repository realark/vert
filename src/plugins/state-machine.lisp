(in-package :recurse.vert)

 (eval-when (:compile-toplevel :load-toplevel :execute)
   (defparameter %state-machine-names% (make-hash-table)
     "Hash class-name -> (state-machine-names-list). Used to initialize state machines after every object initialization."))

@export
(defclass stateful (game-object)
  ((state-machines :initform (make-hash-table)
                   :documentation ":state-machine-name -> :current-state"))
  (:documentation "A game object with various states. Appropriate state actions will be invoked after UPDATE-USER on the game object."))

(defmethod initialize-instance :after ((stateful stateful) &rest args)
  (declare (ignore args))
  (%initialize-all-state-machines stateful))

@export
(defun current-state-for (stateful-game-object state-machine-name)
  "Return the current state (keyword) for STATEFUL-GAME-OBJECT's state-machine with the name of STATE-MACHINE-NAME."
  (declare (stateful stateful-game-object)
           (keyword state-machine-name))
  (gethash state-machine-name (slot-value stateful-game-object 'state-machines)))

;; TODO: Use a setf-able getter/setter for current state
@export
(defgeneric change-state (stateful-game-object delta-t-ms world-context state-machine-name new-state-name)
  (:documentation "Change state for STATEFUL-GAME-OBJECT and run :on-deactivate and :on-active for the old and new states."))

(defgeneric %initialize-all-state-machines (object)
  (:documentation "TODO")
  (:method ((stateful stateful))))

(defgeneric %initialize-state-machine (object state-machine-name)
  (:documentation "TODO"))

(defgeneric %state-machine-update (object state-name delta-t-ms scene)
  (:documentation "Run the :while-active body for the current state of OBJECT"))

(defmethod update-user :after ((stateful stateful) delta-t-ms world-context)
  (with-slots (state-machines) stateful
    (loop :for state-machine-name :being :the hash-keys :of (slot-value stateful 'state-machines) :do
         (%state-machine-update stateful state-machine-name delta-t-ms world-context))))

;; TODO: (defstate state-machine-name initial-state* (arg-lambda-list) body))
@export
(defmacro defstate ((object-binding state-machine-name-keyword delta-t-binding scene-binding &key initial-state old-state-binding new-state-binding) &body states-and-actions)
  "TODO write doc"
  (let ((valid-state-actions '(:on-activate :while-active :on-deactivate)))
    (flet ((validate-binding (binding)
             (unless (or (symbolp binding)
                         (and
                          (list binding)
                          (= 2 (length binding))
                          (every #'symbolp binding)))
               (error "Invalid binding ~A. Must be a symbol or two-value symbol list." binding)))
           (binding-name (binding)
             (if (symbolp binding)
                 binding
                 (elt binding 0)))
           (binding-class (binding)
             (if (symbolp binding)
                 binding
                 (elt binding 1)))
           (validate-states-and-actions (states-and-actions)
             (unless (>= (length states-and-actions) 1)
                     (error "At least one state must be defined for ~A" state-machine-name-keyword))
             (loop :for state-action :in states-and-actions :do
                  (unless (and (listp state-action) (>= (length state-action) 1))
                    (error
                     "Illegal state-action ~A. Expected (:state-action-name &body state-action-body)"
                     state-action))
                  (loop :for action :in (rest state-action) :do
                       (unless (and
                                (listp action)
                                (>= (length action) 1)
                                (find (first action) valid-state-actions))
                         (error "illegal action name ~A. Expected one of ~A"
                                (first action)
                                valid-state-actions)))))
           (get-first-state-name (states-and-actions)
             (first (first states-and-actions)))
           (get-action-body (action-name state-actions)
             (loop :for action :in (rest state-actions) :do
                  (when (eq action-name (first action))
                    (return (rest action)))))
           (state-names (states-and-actions)
             (loop :for state-action :in states-and-actions :collect
                  (first state-action))))
      (validate-binding object-binding)
      (validate-binding delta-t-binding)
      (validate-binding scene-binding)
      (assert (keywordp state-machine-name-keyword))
      (assert (or (null initial-state) (keywordp initial-state)))
      (when (or old-state-binding new-state-binding)
        (error "TODO: implement old and new state bindings for state transitions."))
      (validate-states-and-actions states-and-actions)
      (setf initial-state (or initial-state (get-first-state-name states-and-actions)))

      (alexandria:with-gensyms (current-state new-state-name state-machines machine-name)
        `(progn
           (eval-when (:execute :load-toplevel :compile-toplevel)
             (let* ((class-name (quote ,(binding-class object-binding)))
                    (states (gethash class-name %state-machine-names%)))
               (when (not (find ,state-machine-name-keyword states))
                 (setf (gethash class-name %state-machine-names%)
                       (if states
                           (push ,state-machine-name-keyword states)
                           (list ,state-machine-name-keyword))))))

           (defmethod %initialize-all-state-machines :after (,object-binding)
             (loop :for ,machine-name :in (gethash (quote ,(binding-class object-binding)) %state-machine-names%) :do
                  (%initialize-state-machine ,(binding-name object-binding) ,machine-name)))

           (defmethod %initialize-state-machine (,object-binding (,machine-name (eql ,state-machine-name-keyword)))
             (setf (gethash ,machine-name (slot-value ,(binding-name object-binding) 'state-machines))
                   ,initial-state))

           (defmethod change-state (,object-binding ,delta-t-binding ,scene-binding (,machine-name (eql ,state-machine-name-keyword)) ,new-state-name)
             (with-slots ((,state-machines state-machines)) ,(binding-name object-binding)
               (let ((,current-state (gethash ,machine-name ,state-machines)))
                 (unless (find ,new-state-name '(,@(state-names states-and-actions)))
                   (error "~A is not a valid state. Expected one of ~A"
                          ,new-state-name
                          '(,@(state-names states-and-actions))))
                 (unless (eq ,current-state ,new-state-name)
                   ;; run deactivate on current
                   (ecase ,current-state
                     ,@(loop :for state-action :in states-and-actions :collect
                            `( ,(first state-action)
                                ,@(get-action-body :on-deactivate state-action))))
                   ;; switch state and run activate on new
                   (setf (gethash ,machine-name ,state-machines) ,new-state-name
                         ,current-state ,new-state-name)
                   (ecase ,current-state
                     ,@(loop :for state-action :in states-and-actions :collect
                            `(,(first state-action)
                               ,@(get-action-body :on-activate state-action))))))))

           (defmethod %state-machine-update (,object-binding (,machine-name (eql ,state-machine-name-keyword)) ,delta-t-binding ,scene-binding)
             (let ((,current-state (gethash ,machine-name (slot-value ,(binding-name object-binding) 'state-machines))))
               (ecase ,current-state
                 ,@(loop :for state-action :in states-and-actions :collect
                        `(,(first state-action)
                           ,@(get-action-body :while-active state-action)))))))))))
