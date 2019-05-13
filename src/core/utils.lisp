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
