(in-package :recurse.vert/test)

(defparameter *method-invoke-id* 0)

(defun %create-method-invoke-id ()
  (incf *method-invoke-id*))

(defclass test-introspector ()
  ((introspection-enabled :initarg :introspection-enabled
                          :initform T)
   (invoked-methods :initform '()
                    :accessor invoked-methods
                    :documentation "alist (method-name . invoke-id)"))
  (:documentation "Counts the number of invocations of methods."))

(proclaim '(inline notice-method-invoked))

(defun notice-method-invoked (test-introspector method-name)
  "Inform TEST-INTROSPECTOR that METHOD-NAME was invoked"
  (when (slot-value test-introspector 'introspection-enabled)
    (push (cons method-name (%create-method-invoke-id))
          (invoked-methods test-introspector))))

(defun event-callback-name (event-name)
  "Get the callback method name of a clos-event"
  (concatenate 'string "event-callback-" event-name))

(defun method-invoke-count (test-introspector method-name)
  "Get the invoke count of METHOD-NAME on TEST-INTROSPECTOR"
  (loop with count = 0
     for (method . invoke-id) in (invoked-methods test-introspector) do
       (when (equalp method method-name) (incf count))
     finally (return count)))

(defun method-invoke-id (test-introspector method-name)
  "Get the invoke id for the last invocation of METHOD-NAME on TEST-INTROSPECTOR"
  (cdr (assoc method-name
              (invoked-methods test-introspector)
              :test #'equalp)))

(defun method-invoke-order (&rest introspectors)
  "Return all methods invoked by INTROSPECTORS in the order they were invoked.
Oldest entry comes first."
  (mapcar
   (lambda (entry)
     (car entry))
   (sort (apply #'append
                (mapcar (lambda (introspector)
                          (invoked-methods introspector))
                        introspectors))
         (lambda (entry1 entry2)
           (< (cdr entry1) (cdr entry2))))))
