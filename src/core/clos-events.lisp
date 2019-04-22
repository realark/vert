;;;; A simple event system for adding subscribers to clos methods
(in-package :recurse.vert)

(defclass event-publisher ()
  ((event-subscribers :documentation "even-name-symbol -> array of subs"))
  (:documentation "Event publisher class."))

;; lazy initialize event map
(defmethod slot-unbound (class (object event-publisher) (slot-name (eql 'event-subscribers)))
  (setf (slot-value object 'event-subscribers) (make-hash-table)))

(defmacro add-subscriber (publisher subscriber &rest event-names)
  "Notify SUBSCRIBER of PUBLISHER's events specified by EVENT-NAMES."
  (assert (every #'symbolp event-names))
  (alexandria:once-only (publisher subscriber)
    `(with-slots (event-subscribers) ,publisher
       (loop for event-name in ',event-names do
            (let ((sub-list (gethash event-name event-subscribers)))
              (unless sub-list
                (setf sub-list (make-array 5
                                           :adjustable T
                                           :fill-pointer 0)
                      (gethash event-name event-subscribers) sub-list))
              (unless (find ,subscriber sub-list)
                (vector-push-extend ,subscriber sub-list)))))))

(defmacro remove-subscriber (publisher subscriber &rest event-names)
  "Stop notifying SUBSCRIBER of PUBLISHER's events.
If no EVENT-NAMES are passed, SUBSCRIBER is removed from all of PUBLISHER's events."
  `(with-slots (event-subscribers) ,publisher
     (loop for event-name in ',event-names do
          (setf (gethash event-name event-subscribers)
                (delete ,subscriber (gethash event-name event-subscribers))))))

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
         ,@body)
       (defmethod ,event-name :after ((publisher event-publisher) ,@event-args)
                  (declare (optimize (speed 3)))
                  (loop for sub across (the (array T)
                                            (gethash ',event-name
                                                     (slot-value publisher 'event-subscribers)
                                                     #()))
                     do (,callback-name publisher sub ,@event-args-names)))
       (defmethod ,callback-name (publisher listener ,@event-args)
         (declare (optimize (speed 3) (safety 0)))
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
