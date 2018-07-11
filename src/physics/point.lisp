(in-package :recurse.vert)

(defstruct (point (:constructor make-point-impl))
  "A 3d point"
  (x 0.0 :type world-position)
  (y 0.0 :type world-position)
  (z 0.0 :type world-position))

(defmacro make-point (&rest point-args)
  "construct a point with all values coerced to world-positions"
  `(make-point-impl
    ,@(loop for arg in point-args
         collect (if (keywordp arg)
                     arg
                     `(coerce ,arg 'world-position)))))

(proclaim '(inline distance-between))
(defun distance-between (p1 p2)
  "Compute the distance between P1 and P2."
  (declare (optimize (speed 3)
                     (space 3))
           (point p1 p2))
  (with-accessors ((x1 point-x) (y1 point-y) (z1 point-z)) p1
    (with-accessors ((x2 point-x) (y2 point-y) (z2 point-z)) p2
      (the world-position
           (sqrt (+ (expt (- x1 x2) 2)
                    (expt (- y1 y2) 2)
                    (expt (- z1 z2) 2)))))))

(defparameter *origin* (make-point :x 0 :y 0 :z 0))

(defmethod x ((point point))
  (point-x point))

(defmethod (setf x) (value (point point))
  (setf (point-x point) (coerce value 'world-position)))

(defmethod y ((point point))
  (point-y point))

(defmethod (setf y) (value (point point))
  (setf (point-y point) (coerce value 'world-position)))

(defmethod z ((point point))
  (point-z point))

(defmethod (setf z) (value (point point))
  (setf (point-z point) (coerce value 'world-position)))
