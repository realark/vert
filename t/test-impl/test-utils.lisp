(in-package :recurse.vert/test)

(defun float= (real1 real2 &key (precision 5))
  "Superset of #'=. T if (= real1 real2) or reals are floats within PRECISION."
  (declare (real real1 real2)
           (fixnum precision))
  (or (= real1 real2)
      (= 0 (truncate (* (expt 10 precision) (- real1 real2))))))

(defun point= (point1 point2)
  (declare (type point point1 point2))
  (float= 0 (distance-between point1 point2)))

(defmethod move (game-object delta-x delta-y &optional (delta-z 0))
  (declare (type game-object game-object))
  (with-accessors ((x x) (y y) (z z)) game-object
    (incf x delta-x)
    (incf y delta-y)
    (incf z delta-z)
    (values)))

(defun default-test-fn (obj1 obj2)
  ;; Compare numbers by float value out to 5 decimal places
  (if (and (realp obj1) (realp obj2))
      (float= obj1 obj2 :precision 5)
      (equal obj1 obj2)))

(setf prove:*default-test-function*
      #'default-test-fn)
