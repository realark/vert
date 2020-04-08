(in-package :recurse.vert)
;;;; generic scheduler for running callbacks at arbitrary times

@export-class
(defclass scheduler ()
  ((timer-fn :initform #'ticks
             :initarg :timer-fn
             :documentation "Zero arg fn. Returns a fixnum timestamp starting from an arbitrary point in time.")
   (next-event-timestamp :initform nil
                         :reader scheduler-next-event-timestamp
                         :documentation "timestamp of the next event to run.")
   (callbacks :initform (make-array 10
                                    :fill-pointer 0
                                    :adjustable t
                                    :initial-element nil)
              :documentation "key-value plist-vector of (timestamp zero-arg-fn). When scheduler's time equals or exceeds the timestamp, the lambda will be invoked.")))

@export
(defmethod scheduler-add ((scheduler scheduler) timestamp zero-arg-fn)
  "When SCHEDULER's timer equals or exceeds TIMESTAMP the ZERO-ARG-FN callback will be invoked."
  (declare (optimize (speed 3))
           (scheduler scheduler)
           ((function ()) zero-arg-fn)
           (fixnum timestamp))
  (with-slots (callbacks) scheduler
    (declare (vector callbacks))
    (loop :for i :from 0 :below (length callbacks) :by 2 :do
         (unless (elt callbacks i)
           (setf (elt callbacks i) timestamp
                 (elt callbacks (+ i 1)) zero-arg-fn)
           (return))
       :finally
         (vector-push-extend timestamp callbacks)
         (vector-push-extend zero-arg-fn callbacks))
    zero-arg-fn)
  (with-slots (next-event-timestamp) scheduler
    (declare ((or null fixnum) next-event-timestamp))
    (when (or (null next-event-timestamp)
              (< timestamp next-event-timestamp))
      (setf next-event-timestamp timestamp))))

@export
(defmethod scheduler-run-callbacks ((scheduler scheduler))
  "Run any callbacks scheduled to run now (or before now if they haven't been run yet)."
  (declare (optimize (speed 3))
           (scheduler scheduler))
  (with-slots (callbacks timer-fn next-event-timestamp) scheduler
    (declare (vector callbacks)
             ((or null fixnum) next-event-timestamp)
             ((function () fixnum) timer-fn))

    (setf next-event-timestamp nil)
    (loop :with now = (funcall timer-fn)
       :for i :from 0 :below (length callbacks) :by 2 :do
         (when (elt callbacks i)
           (let ((time-to-run (elt callbacks i))
                 (callback (elt callbacks (+ i 1))))
             (declare ((function ()) callback)
                      (fixnum time-to-run now))
             (if (>= now time-to-run)
                 ;; run the callback if past now
                 (progn
                   (funcall callback)
                   (setf (elt callbacks i) nil
                         (elt callbacks (+ 1 i)) nil))
                 ;; ahead of now, update the next timestamp
                 (when (or (null next-event-timestamp)
                           (< time-to-run next-event-timestamp))
                   (setf next-event-timestamp time-to-run))))))
    (values)))

@export
(defmethod scheduler-flush-all ((scheduler scheduler))
  "Cancel all pending jobs in SCHEDULER."
  (declare (optimize (speed 3))
           (scheduler scheduler))
  (setf (fill-pointer (slot-value scheduler 'callbacks)) 0
        (slot-value scheduler 'next-event-timestamp) nil)
  scheduler)

@export
(defmethod scheduler-now ((scheduler scheduler))
  (declare (optimize (speed 3)))
  (with-slots (timer-fn) scheduler
    (declare ((function () fixnum) timer-fn))
    (funcall timer-fn)))

;;;; thread-safe-scheduler

@export-class
(defclass thread-safe-scheduler (scheduler)
  ((lock :initform (bt:make-recursive-lock "thread-safe-scheduler")))
  (:documentation "A scheduler which locks around add and remove operations."))

(defmethod scheduler-add :around ((scheduler thread-safe-scheduler) timestamp zero-arg-fn)
  (bt:with-recursive-lock-held ((slot-value scheduler 'lock))
    (call-next-method scheduler timestamp zero-arg-fn)))

(defmethod scheduler-run-callbacks :around ((scheduler thread-safe-scheduler))
  (bt:with-recursive-lock-held ((slot-value scheduler 'lock))
    (call-next-method scheduler)))

(defmethod scheduler-flush-all :around ((scheduler thread-safe-scheduler))
  (bt:with-recursive-lock-held ((slot-value scheduler 'lock))
    (call-next-method scheduler)))

(defmethod scheduler-next-event-timestamp :around ((scheduler thread-safe-scheduler))
  (bt:with-recursive-lock-held ((slot-value scheduler 'lock))
    (call-next-method scheduler)))
