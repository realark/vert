;;;; A simple event system for adding subscribers to clos methods
(in-package :recurse.vert)

(defclass event-publisher ()
  ((event-subscribers :documentation "event-name-symbol -> (weak) array of subs"))
  (:documentation "Event publisher class."))

;; lazy initialize event map
(defmethod slot-unbound (class (object event-publisher) (slot-name (eql 'event-subscribers)))
  (setf (slot-value object 'event-subscribers) (make-hash-table :test #'eq)))

(defmacro add-subscriber (publisher subscriber &rest event-names)
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
                             sub-list (simple-array-double-size
                                       sub-list
                                       :new-array (sb-ext:make-weak-vector (* 2 (length sub-list))
                                                                           :initial-element nil))))
                     (setf (elt sub-list first-null-index)
                           ,subscriber))))))))

(defmacro remove-subscriber (publisher subscriber &rest event-names)
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

(defun %event-callback-name (event-name)
  (merge-symbols (symbol-package event-name) 'event-callback- event-name))

(defun %is-exported (symbol)
  (multiple-value-bind (_ visibility)
      (find-symbol (symbol-name symbol)
                   (symbol-package symbol))
    (declare (ignore _))
    (eq :external visibility)))

(defun %export-callback (event-name)
  (export (list (%event-callback-name event-name))
          (symbol-package event-name)))

(defun %event-args-names (event-args)
  (mapcar (lambda (arg)
            (if (listp arg)
                (first arg)
                arg))
          event-args))

(defmacro defevent (event-name (publisher &rest event-args)
                    &optional (doc-string "") &body body)
  (when (%is-exported event-name)
    (%export-callback event-name))
  (let ((callback-name (%event-callback-name event-name))
        (event-args-names (%event-args-names event-args)))
    `(progn
       (defgeneric ,event-name (publisher ,@event-args-names)
         (:documentation ,(format nil "(clos-event method) ~A" doc-string)))
       (defgeneric ,callback-name (publisher subscriber ,@event-args-names)
         (:documentation ,(format nil "(clos-event callback) ~A" doc-string)))
       (defmethod ,event-name (,publisher ,@event-args)
         (declare (optimize (speed 3)))
         ,@body)
       (defmethod ,event-name :after ((publisher event-publisher) ,@event-args)
                  (declare (optimize (speed 3)))
                  (loop :for sub :across
                       (the (simple-array (or null event-publisher))
                            (gethash ',event-name
                                     (slot-value publisher 'event-subscribers)
                                     #()))
                     :do
                       (when sub
                         (,callback-name publisher sub ,@event-args-names))))
       (defmethod ,callback-name (publisher listener ,@event-args)
         (declare (optimize (speed 3)))
         (values)))))

(defmacro fire-event (publisher event-name &rest event-args)
  "Invokes the method EVENT-NAME on PUBLISHER and EVENT-ARGS."
  `(,event-name ,publisher ,@event-args))

@export
(defmacro defevent-callback (event-name (publisher subscriber &rest event-args) &body body)
  ;; TODO doc
  (let ((callback-name (%event-callback-name event-name)))
    `(progn
       (handler-bind ((error
                       (lambda (e)
                         (declare (ignore e))
                         (error (format
                                 nil
                                 "event-callback method ~A is undefined."
                                 (quote ,callback-name))))))
         ;; ensure that listen event has been defined
         (function ,callback-name))
       (defmethod ,callback-name (,publisher ,subscriber ,@event-args)
         ,@body))))
