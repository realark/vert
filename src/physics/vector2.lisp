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

@export
(defun scale-vector2 (vector2 scalar)
  "Create a scaled vector2"
  (compute-scale-vector2 vector2 scalar (make-vector2)))

@export
(defun compute-scale-vector2 (vector2 scalar result-vector)
  "Compute a scaled vector2. Results are stored in RESULT-VECTOR2 and returned."
  (setf (x result-vector) (* (vector-x vector2) scalar)
        (y result-vector) (* (vector-y vector2) scalar))
  result-vector)

@export
(defun compute-magnitude (vector2)
  "compute the magnitude of vector2"
  (sqrt (+ (expt (vector-x vector2) 2)
           (expt (vector-y vector2) 2))))

@export
(defun compute-normalized-vector2 (p1 p2 result-vector)
  "Compute a normalized vector (= magnitude 1) which points from p1 towards p2.
VECTOR is updated with the computed values and returned"
  (with-accessors ((x1 point-x) (y1 point-y)) p1
    (with-accessors ((x2 point-x) (y2 point-y)) p2
      (setf (x result-vector) (- x2 x1)
            (y result-vector) (- y2 y1))
      (let* ((magnitude (compute-magnitude result-vector)))
        (compute-scale-vector2 result-vector (/ 1.0 magnitude) result-vector)))))

@export
(defun make-normalized-vector2 (p1 p2)
  "Create a normalized vector (= magnitude 1) which points from p1 towards p2"
  (compute-normalized-vector2 p1 p2 (make-vector2)))
