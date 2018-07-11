(in-package :recurse.vert/test)

(defclass test-event-subscriber (test-introspector)
  ((last-arg
    :initform nil
    :reader last-arg
    :documentation "Last value of 'arg' seen in the event callback")))

(defevent-callback simple-event ((publisher test-event-publisher) (subscriber test-event-subscriber))
  (notice-method-invoked subscriber (event-callback-name "simple-event")))

(defevent-callback method-event ((publisher test-event-publisher) (subscriber test-event-subscriber) arg)
  (setf (slot-value subscriber 'last-arg) arg)
  (notice-method-invoked subscriber (event-callback-name "method-event")))
