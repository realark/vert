;; 2d Vector
(in-package :recurse.vert)

(defstruct (vector2 (:conc-name vector-))
  "A 2d vector with X and Y dimensions"
  (x 0.0 :type vector-dimension)
  (y 0.0 :type vector-dimension))

(defmethod x ((vector vector2))
  (vector-x vector))

(defmethod (setf x) (value (vector vector2))
  (setf (vector-x vector) (coerce value 'vector-dimension)))

(defmethod y ((vector vector2))
  (vector-y vector))

(defmethod (setf y) (value (vector vector2))
  (setf (vector-y vector) (coerce value 'vector-dimension)))

(defun make-velocity-vector-seconds (&key (x 0.0) (y 0.0))
  (make-vector2 :x (/ x 1000.0) :y (/ y 1000.0)))

(defun make-acceleration-vector-seconds (&key (x 0) (y 0))
  (make-vector2 :x (/ x (expt 1000.0 2)) :y (/ y (expt 1000.0 2))))

(defun scale-vector2 (vector2 scalar)
  (make-vector2
   :x (* (vector-x vector2) scalar)
   :y (* (vector-y vector2) scalar)))

(defun compute-magnitude (vector2)
  "compute the magnitude of vector2"
  (sqrt (+ (expt (vector-x vector2) 2)
           (expt (vector-y vector2) 2))))

(defun make-normalized-vector2 (p1 p2)
  "Create a normalized vector (= magnitude 1) which points from p1 towards p2"
  (with-accessors ((x1 point-x) (y1 point-y)) p1
    (with-accessors ((x2 point-x) (y2 point-y)) p2
      (let* ((un-normalized-vector (make-vector2
                                    :x (- x2 x1)
                                    :y (- y2 y1)))
             (magnitude (compute-magnitude un-normalized-vector)))
        (scale-vector2 un-normalized-vector (/ 1.0 magnitude))))))
