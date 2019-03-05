;;;; Misc utilities. Nothing complicated.

(in-package :recurse.vert)

@export
(defconstant float-pi (float pi 0f0)
  "Pi represented as a single slot float")

@export
(defconstant tau (float (* 2 pi) 0f0)
  "Tau represented as a single slot float")

(defun deg->rad (x) (* x (/ float-pi 180)))
(defun rad->deg (x) (* x (/ 180 float-pi)))

(defmacro ticks ()
  "Wallclock millisecond timestamp starting from an arbitrary point in time."
  `(sdl2:get-ticks))

@export
(defun ticks-nanos ()
  "Wallclock nanosecond timestamp starting from an arbitrary point in time."
  (multiple-value-bind (sec microsec) (sb-ext:get-time-of-day)
    (+ (* sec #.(expt 10 9)) (* microsec #.(expt 10 3)))))

(defun merge-symbols (package &rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))
          package))

(defmacro list-contains-p (list item)
  "Non-nil if list contains item"
  `(find ,item ,list))

(defun insert-after (list index item)
  "Return a copy of LIST with ITEM inserted after INDEX.

If INDEX exceeds the length of the lists or is less
than 0 ITEM will be appended to the end or beginning
of the LIST copy."
  (cond
    ((< index 0) (append (list item) list))
    ((> index (length list)) (append list (list item)))
    (T (let ((cpy (copy-list list)))
         (push item (cdr (nthcdr index cpy)))
         cpy))))

(defun insert-sorted (list item comparator-fn)
  "Insert ITEM into a sorted LIST using COMPARATOR-FN.

COMPARATOR-FN == one arg function that returns T if ITEM is greater than the arg."
  (let ((insertion-index -1))
    (loop with element = (first list)
       while (and (< (+ 1 insertion-index) (length list))
                  (funcall comparator-fn element))
       do
         (incf insertion-index)
         (setf element (nth (+ 1 insertion-index) list))
       finally (return (insert-after list insertion-index item)))))

(defmacro notes (&body body)
  "Macro for writing un-eval'd notes and pseudo code."
  (declare (ignore body))
  '())
