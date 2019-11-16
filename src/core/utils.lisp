;;;; Misc utilities. Nothing complicated.

(in-package :recurse.vert)

(declaim (ftype (function () (unsigned-byte 64)) ticks-nanos ticks))

@export
(defun ticks ()
  "Wallclock millisecond timestamp starting from an arbitrary point in time."
  (declare (optimize (speed 3)
                     (safety 0)))
  (truncate (ticks-nanos) #.(expt 10 6)))

@export
(defun ticks-nanos ()
  "Wallclock nanosecond timestamp starting from an arbitrary point in time."
  (declare (optimize (speed 3)
                     (safety 0)))
  (multiple-value-bind (sec microsec) (sb-ext:get-time-of-day)
    (declare (fixnum sec microsec))
    (+ (the fixnum (* sec #.(expt 10 9)))
       (the fixnum (* microsec #.(expt 10 3))))))

(defun merge-symbols (package &rest symbols)
  "intern a symbol in PACKAGE with the concatenated symbol-name of all SYMBOLS."
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))
          package))

(defmacro runtime-type-assert (form expected-type &optional error-message)
  "Wrap FORM in a type assertion. The result of FORM will be returned if the result is of type EXPECTED-TYPE."
  (alexandria:once-only (form expected-type error-message)
    `(if (typep ,form ,expected-type)
         ,form
         (error
          (format nil
                  "~A~A must be of type ~A. Got ~A"
                  (if ,error-message
                      (format nil "~A: " ,error-message)
                      "")
                  ,form
                  ,expected-type
                  (type-of ,form))))))

@export
(defun array-insert-at-index (array i object)
  "Insert OBJECT into ARRAY at index I.
Element I and all subsequent elements will be right-shifted."
  (declare (optimize (speed 3))
           (array array)
           (fixnum i))
  (assert (and (>= i 0)
               (array-has-fill-pointer-p array)
               (>= (length array) i)))
  (vector-push-extend object array)
  (loop :for n :from (- (length array) 1) :downto (+ i 1) :do
       (setf (elt array n) (elt array (- n 1)))
     :finally (setf (elt array i) object))
  array)

@export
(defun array-insert-sorted (array object predicate)
  "Insert OBJECT into ARRAY in the sorted position defined by the two-arg PREDICATE function.
Assumes ARRAY is initially sorted."
  (declare (optimize (speed 3)))
  (labels ((find-insertion-index (array object predicate &optional beg end)
             (declare (array array))
             (unless beg
               (setf beg 0))
             (unless end
               (setf end (max 0 (- (length array) 1))))
             (let ((half (+ beg (floor (/ (- end beg) 2)))))
               (declare ((integer 0 *) beg end half))
               (cond ((>= beg end) end)
                     ((funcall predicate
                               object
                               (elt array beg))
                      beg)
                     ((funcall predicate
                               (elt array end)
                               object)
                      (+ end 1))
                     ((funcall predicate
                               object
                               (elt array half))
                      (find-insertion-index array object predicate beg half))
                     ((funcall predicate
                               (elt array half)
                               object)
                      (find-insertion-index array object predicate (+ half 1) end))
                     (t half)))))
    (array-insert-at-index array (find-insertion-index array object predicate) object)))

(eval-when (:load-toplevel :execute)
  (defvar *engine-start-hooks*
    (make-hash-table)
    "zero arg functions to invoke when the engine stops.")

  (defvar *engine-stop-hooks*
    (make-hash-table)
    "zero arg functions to invoke when the engine stops."))

(defmacro on-engine-start ((label) &body body)
  "Executed BODY once each time the engine starts.
LABEL must be symbol. Previously bound label code will be replaced if the same label is used twice."
  `(eval-when (:load-toplevel :execute)
     (setf (gethash ,label *engine-start-hooks*)
           (lambda ()
             ,@body))))

(defmacro on-engine-stop ((label) &body body)
  "Executed BODY once each time the engine stops.
LABEL must be symbol. Previously bound label code will be replaced if the same label is used twice."
  `(eval-when (:load-toplevel :execute)
     (setf (gethash ,label *engine-stop-hooks*)
           (lambda ()
             ,@body))))
