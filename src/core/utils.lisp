;;;; Misc utilities. Nothing complicated.

(in-package :recurse.vert)

@export
(defconstant float-pi (float pi 0f0)
  "Pi represented as a single slot float")

@export
(defconstant tau (float (* 2 pi) 0f0)
  "Tau represented as a single slot float")

@export
(defun deg->rad (x) (* x (/ float-pi 180)))
@export
(defun rad->deg (x) (* x (/ 180 float-pi)))

@export
(defmacro ticks ()
  "Wallclock millisecond timestamp starting from an arbitrary point in time."
  `(sdl2:get-ticks))

@export
(defun ticks-nanos ()
  "Wallclock nanosecond timestamp starting from an arbitrary point in time."
  (multiple-value-bind (sec microsec) (sb-ext:get-time-of-day)
    (+ (* sec #.(expt 10 9)) (* microsec #.(expt 10 3)))))

(defun merge-symbols (package &rest symbols)
  "intern a symbol in PACKAGE with the concatenated symbol-name of all SYMBOLS."
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))
          package))

@export
(defmacro null-fallback (object fallback-form)
  "If OBJECT is non-nil, return it. Otherwise eval and return FALLBACK-FORM."
  (alexandria:once-only (object)
    `(if (null ,object)
         ,fallback-form
         ,object)))

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

@export
(defun center-object-xy (object target)
  "Update OBJECT's X and Y positions to be centered inside of TARGET"
  (with-accessors ((x1 x) (y1 y) (w1 width) (h1 height)) object
    (with-accessors ((x2 x) (y2 y) (w2 width) (h2 height)) target
      (setf x1 (- (+ x2 (/ w2 2)) (/ w1 2))
            y1 (- (+ y2 (/ h2 2)) (/ h1 2))))))
