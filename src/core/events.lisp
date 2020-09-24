(in-package :recurse.vert)

(defclass event-publisher ()
  ((event-subscribers :documentation "event-name-symbol -> (weak) array of subs"))
  (:documentation "An object which may publish events"))

;; lazy initialize event map
(defmethod slot-unbound (class (object event-publisher) (slot-name (eql 'event-subscribers)))
  (setf (slot-value object 'event-subscribers) (make-hash-table :test #'eq)))

;;; implementation

(defclass %event ()
  ((publisher :initarg :publisher
              :initform (error ":publisher required"))
   (name :initarg :name
         :type symbol
         :initform (error ":name required"))
   (args :initarg :args
         :initform (make-array 0 :adjustable t))))

(defmethod print-object ((event %event) out)
  (with-slots (publisher name args) event
    (print-unreadable-object (event out :type t)
      (format out "(~A -> ~A ~A)" publisher name args))))

;;; %pending-events% and %event-bus% are swapped at the start of every update frame
;;; and the new %pending-events% is truncated

(declaim ((vector %event) %pending-events%))
(defvar %pending-events%
  (make-array 128 :fill-pointer 0 :adjustable t)
  "Events which will be published next frame.")

(declaim ((vector %event) %event-bus%))
(defvar %event-bus%
  (make-array 128 :fill-pointer 0 :adjustable t)
  "Events which have been published in the previous update frame.")

(declaim ((vector %event) %event-object-pool%))
(defvar %event-object-pool%
  (make-array 128 :fill-pointer 0 :adjustable t)
  "Object pool for event instances to reduce GC caused by frequent events firing every frame.")

@export
(defmacro do-events ((event-name event-publisher event-args) &body body)
  "Iterate all events published in the previous frame. Invoked RETURN to stop iteration.
NOTE: frequent scans of the event bus can prove costly. Please consider using event handlers instead of this method."
  (declare (symbol event-name event-publisher event-args))
  (alexandria:with-gensyms (event-obj)
    `(loop :for ,event-obj :across %event-bus% :do
      (with-slots ((,event-publisher publisher) (,event-name name) (,event-args args)) ,event-obj
        ,@body))))

(defun events-flush ()
  "Clear all events without running any callbacks."
  (loop :for old-event :across %pending-events% :do
    (with-slots (name publisher args) old-event
      (array-null-out args)
      (setf name 'recycled
            publisher nil
            (fill-pointer args) 0))
    (vector-push-extend old-event %event-object-pool%)
        :finally (setf (fill-pointer %pending-events%) 0))

  (loop :for old-event :across %event-bus% :do
    (with-slots (name publisher args) old-event
      (array-null-out args)
      (setf name 'recycled
            publisher nil
            (fill-pointer args) 0))
    (vector-push-extend old-event %event-object-pool%)
        :finally (setf (fill-pointer %event-bus%) 0)))

(defun events-run-pending ()
  "Run events from the previous frame. Called at the beginning of an update frame.
Do not call this function from game code. It will be invoked by the engine at the beginning of the update frame."
  (declare (optimize (speed 3)))
  (block swap-buffers-and-truncate-pending-events
    (rotatef %event-bus% %pending-events%)
    ;; explicitly null out old events and args so we don't hold a strong ref
    ;; NOTE if this proves inefficient consider only doing this when the engine does an explicit GC
    (loop :for old-event :across %pending-events% :do
      (with-slots (name publisher args) old-event
        (array-null-out args)
        (setf name 'recycled
              publisher nil
              (fill-pointer args) 0))
      (vector-push-extend old-event %event-object-pool%)
          :finally (setf (fill-pointer %pending-events%) 0)))

  (do-events (name publisher args)
    (%event-pub name publisher args)
    (when (typep publisher 'event-publisher)
      (with-slots ((subs event-subscribers)) publisher
        (loop :for sub :across (the vector (gethash name subs #())) :do
          (%event-sub name publisher sub args))))))

;;;; Basic Event pub/sub api

(defgeneric %event-pub (event-name publisher &optional event-args)
  (:documentation "clos impl of event pub method dispatch. Event pub methods will specialize on this method")
  (:method (event-name publisher &optional event-args)
    (declare (optimize (speed 3))
             (ignore event-name publisher event-args))
    ;; no-op
    (values)))

(defgeneric %event-sub (event-name pub sub &optional event-args)
  (:documentation "clos impl of event sub method dispatch. Event handler methods will specialize on this method")
  (:method (event-name pub sub &optional event-args)
    (declare (optimize (speed 3))
             (ignore event-name pub sub event-args))))

@export
(defmacro defevent (event-name (publisher &rest event-args)
                    &optional doc-string &body body)
  "An action which runs once, before any subscriber handlers are invoked."
  (declare ((or null string) doc-string)
           (list event-args)
           ((or symbol (cons symbol)) publisher)
           (symbol event-name))
  (assert (every #'symbolp event-args))
  (when (listp publisher) (assert (= (length publisher) 2)))
  `(progn
     (unless (or (null ,doc-string)
                 (documentation ',event-name 'variable))
       (setf (documentation ',event-name 'variable)
             ,doc-string))
     (defmethod %event-pub ((event-name (eql ',event-name)) ,publisher &optional event-args)
       (let (,@(loop :with i = -1
                     :for arg :in event-args :collect
                     `(,arg (elt event-args ,(incf i)))))
         ,@body))
     ',event-name))

@export
(defmacro event-publish (name publisher &rest args)
  "Publish EVENT and run callbacks in the next update frame."
  `(%event-publish (optional-quote ,name) ,publisher ,@args))

(defun %event-publish (name publisher &rest args)
  (declare (optimize (speed 3))
           ((not null) publisher)
           (symbol name)
           (dynamic-extent args))
  (let ((event ; recycle from obj pool, or make a new event instance
          (cond ((> (length %event-object-pool%) 0)
                 (vector-pop %event-object-pool%))
                (t (make-instance '%event
                                  :name nil
                                  :publisher nil
                                  :args (make-array (length args)
                                                    :fill-pointer 0
                                                    :adjustable t
                                                    :initial-contents args))))))
    (with-slots ((n name) (p publisher) (a args)) event
      (vert::array-null-out a)
      (setf n name
            p publisher
            (fill-pointer a) 0)
      (loop :for arg :in args :do
        (vector-push-extend arg a)))
    (vector-push-extend event %pending-events%)))

@export
(defmacro defevent-handler (event-name (pub sub &rest event-args)
                            &optional doc-string
                            &body body)
  "An action which runs once per subscriber to an event"
  (declare (symbol event-name)
           ((or null string) doc-string)
           ((or symbol (cons symbol)) pub sub)
           (list event-args))
  (assert (every #'symbolp event-args))
  (when (listp pub) (assert (= (length pub) 2)))
  (when (listp sub) (assert (= (length sub) 2)))
  `(progn
     (unless (or (null ,doc-string)
                 (documentation ',event-name 'variable))
       (setf (documentation ',event-name 'variable)
             ,doc-string))
     (defmethod %event-sub ((event-name (eql ',event-name)) ,pub ,sub &optional event-args)
       (declare (ignorable event-args))
       (let (,@(loop :with i = -1
                     :for arg :in event-args :collect
                     `(,arg (elt event-args ,(incf i)))))
         ,@body))

     ',event-name))

@export
(defmacro event-subscribe (publisher subscriber &rest event-names)
  "Notify SUBSCRIBER of PUBLISHER's events specified by EVENT-NAMES."
  (assert (every #'symbolp event-names))
  (alexandria:once-only (publisher subscriber)
    `(with-slots (event-subscribers) ,publisher
       (loop :for event-name :across #(,@event-names) :do
            (let ((sub-list (gethash event-name event-subscribers)))
              (unless sub-list
                (setf sub-list (sb-ext:make-weak-vector 1 :initial-element nil)
                      (gethash event-name event-subscribers) sub-list))
              (locally (declare ((simple-array) sub-list))
                (loop :with first-null-index = nil
                   :for i :from 0 :below (length sub-list) :do
                     (let ((sub (elt sub-list i)))
                       (cond ((and (null first-null-index)
                                   (null sub))
                              (setf first-null-index i))
                             ((eq sub ,subscriber)
                              ;; already in sub list. Stop.
                              (return))))
                   :finally
                   ;; not found in sub list
                     (unless first-null-index
                       ;; resize list if no empty spaces
                       (setf first-null-index (length sub-list)
                             sub-list (recurse.vert::simple-array-double-size
                                       sub-list
                                       :new-array (sb-ext:make-weak-vector (* 2 (length sub-list))
                                                                           :initial-element nil))
                             (gethash event-name event-subscribers) sub-list))
                     (setf (elt sub-list first-null-index)
                           ,subscriber))))))))

@export
(defmacro event-unsubscribe (publisher subscriber &rest event-names)
  "Stop notifying SUBSCRIBER of PUBLISHER's events."
  (assert (every #'symbolp event-names))
  `(with-slots (event-subscribers) ,publisher
     (loop :for event-name :across #(,@event-names) :do
       (let ((sub-list (gethash event-name event-subscribers)))
         (when sub-list
           (locally (declare ((simple-array) sub-list))
             (loop :for i :from 0 :below (length sub-list) :do
               (when (eq ,subscriber (elt sub-list i))
                 (setf (elt sub-list i) nil)
                 (return)))))))))

;;;; Global Subscribers

@export
(defmacro defevent-handler-global (global-handler-name (event-name publisher &rest event-args) &optional doc-string &body body)
  "Define a global handler to run after specific event name and publisher types"
  (declare (symbol global-handler-name event-name)
           ((or symbol (cons symbol)) publisher)
           (list event-args)
           ((or null string) doc-string))
  (assert (every #'symbolp event-args))
  (when (listp publisher) (assert (= (length publisher) 2)))
  `(progn
     (unless (or (null ,doc-string)
                 (documentation ',global-handler-name 'variable))
       (setf (documentation ',global-handler-name 'variable)
             ,doc-string))
     (defmethod %event-pub :after ((event-name (eql ',event-name)) ,publisher &optional event-args)
       (let (,@(loop :with i = -1
                     :for arg :in event-args :collect
                     `(,arg (elt event-args ,(incf i)))))
         ,@body))
     ',global-handler-name))
