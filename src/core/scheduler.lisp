(in-package :recurse.vert)
;;;; generic scheduler for running callbacks at arbitrary times

(defclass scheduler ()
  ((timer-fn :initform #'ticks
             :initarg :timer-fn
             :documentation "Zero arg fn. Returns a fixnum timestamp starting from an arbitrary point in time.")
   (callbacks :initform (make-array 10
                                    :fill-pointer 0
                                    :adjustable T
                                    :initial-element nil)
              :documentation "key-value plist-vector of (timestamp zero-arg-fn). When scheduler's time equals or exceeds the timestamp, the lambda will be invoked.")))

@export
(defun scheduler-add (scheduler timestamp zero-arg-fn)
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
    (values)))

@export
(defun scheduler-cancel (zero-arg-fn &key (error-if-not-scheduled T))
  (declare (ignore zero-arg-fn error-if-not-scheduled))
  (error "TODO"))

(defun scheduler-run-callbacks (scheduler)
  (declare (optimize (speed 3))
           (scheduler scheduler))
  (with-slots (callbacks timer-fn) scheduler
    (declare (vector callbacks)
             ((function () fixnum) timer-fn))
    (loop :with now = (funcall timer-fn)
       :for i :from 0 :below (length callbacks) :by 2 :do
         (when (elt callbacks i)
           (let ((time-to-run (elt callbacks i))
                 (callback (elt callbacks (+ i 1))))
             (declare ((function ()) callback)
                      (fixnum time-to-run now))
             (when (>= now time-to-run)
               (funcall callback)
               (setf (elt callbacks i) nil
                     (elt callbacks (+ 1 i)) nil)))))
    (values)))
