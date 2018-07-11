(in-package :recurse.vert/test)

(defclass test-event-publisher (event-publisher test-introspector)
  ())

(defevent simple-event ((publisher test-event-publisher))
    "A simple event with no method body")

(defevent method-event ((publisher test-event-publisher) (arg number))
    "An event with a method body. Returns 1+ ARG"
  (notice-method-invoked publisher "method-event")
  (1+ arg))

(defmethod method-event :before ((publisher test-event-publisher) (arg number))
  (notice-method-invoked publisher "before:method-event"))

(defmethod method-event :after ((publisher test-event-publisher) (arg number))
  (notice-method-invoked publisher "after:method-event"))

(defmethod method-event :around ((publisher test-event-publisher) (arg number))
  (notice-method-invoked publisher "around:preamble:method-event")
  (let ((return-val (call-next-method publisher arg)))
    (notice-method-invoked publisher "around:postamble:method-event")
    return-val))
