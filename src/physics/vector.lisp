;; 2d Vector
(in-package :recurse.vert)

(eval-when (:compile-toplevel :load-toplevel :execute)
  @export
  (deftype vector2 ()
    "2d vector"
    `(simple-array single-float (2)))

  @export
  (deftype vector3 ()
    "3d vector"
    `(simple-array single-float (3)))

  @inline
  (defun vector2 (&optional (x 0.0) (y 0.0))
    (declare (single-float x y))
    (make-array 2
                :element-type 'single-float
                :initial-contents (list x y)))

  @inline
  (defun vector3 (&optional (x 0.0) (y 0.0) (z 0.0))
    (declare (single-float x y z))
    (make-array 3
                :element-type 'single-float
                :initial-contents (list x y z))))

@inline
(defun copy-vector (vector)
  (declare ((or vector2 vector3) vector))
  (if (typep vector 'vector2)
      (vector2 (x vector)
               (y vector))
      (vector3 (x vector)
               (y vector)
               (z vector))))

(defvar *origin* (vector3 0.0 0.0 0.0))

(defmethod x ((vector simple-array))
  (elt vector 0))
(defmethod (setf x) (value (vector simple-array))
  (setf (elt vector 0) value))

(defmethod y ((vector simple-array))
  (elt vector 1))
(defmethod (setf y) (value (vector simple-array))
  (setf (elt vector 1) value))

(defmethod z ((vector simple-array))
  (elt vector 2))
(defmethod (setf z) (value (vector simple-array))
  (setf (elt vector 2) value))

(defmethod width ((vector simple-array))
  (x vector))
(defmethod (setf width) (value (vector simple-array))
  (setf (x vector) value))

(defmethod height ((vector simple-array))
  (y vector))
(defmethod (setf height) (value (vector simple-array))
  (setf (y vector) value))

(defmethod depth ((vector simple-array))
  (z vector))
(defmethod (setf depth) (value (vector simple-array))
  (setf (z vector) value))

@inline
(defun make-velocity-vector-seconds (&key (x 0.0) (y 0.0))
  (vector2 (/ x 1000.0) (/ y 1000.0)))

@inline
(defun make-acceleration-vector-seconds (&key (x 0) (y 0))
  (vector2 (/ x (expt 1000.0 2)) (/ y (expt 1000.0 2))))

@export
@inline
(defun scale-vector (vector scalar)
  "Construct a new vector by multiplying VECTOR by SCALAR"
  (declare (single-float scalar)
           ((or vector2 vector3) vector))
  (if (typep vector 'vector2)
      (vector2 (* (x vector) scalar)
               (* (y vector) scalar))
      (vector3 (* (x vector) scalar)
               (* (y vector) scalar)
               (* (z vector) scalar))))

@export
(defun compute-magnitude (vector)
  "compute the magnitude of vector"
  (declare ((or vector2 vector3) vector))
  (if (typep vector 'vector2)
      (sqrt (+ (expt (x vector) 2)
               (expt (y vector) 2)))
      (sqrt (+ (expt (x vector) 2)
               (expt (y vector) 2)
               (expt (z vector) 2)))))

@export
@inline
(defun normalized-vector2 (p1 p2)
  "Construct a normalized vector2 (= magnitude 1) which points from p1 towards p2."
  (with-accessors ((x1 x) (y1 y)) p1
    (with-accessors ((x2 x) (y2 y)) p2
      (let* ((delta (vector2 (- x2 x1)
                             (- y2 y1)))
             (magnitude (compute-magnitude delta)))
        (declare (dynamic-extent delta))
        (scale-vector delta (/ 1.0 magnitude))))))

@export
@inline
(defun vector= (vector1 vector2)
  "T if values of vector1 and vector2 are equal (as specified by FLOAT= function)."
  (declare (optimize (speed 3))
           (vector2 vector1 vector2))
  (and (float= (x vector1) (x vector2))
       (float= (y vector1) (y vector2))))

@export
@inline
(defun normalized-vector3 (p1 p2)
  "Construct a normalized vector2 (= magnitude 1) which points from p1 towards p2."
  (with-accessors ((x1 x) (y1 y) (z1 z)) p1
    (with-accessors ((x2 x) (y2 y) (z2 z)) p2
      (let* ((delta (vector3 (- x2 x1)
                             (- y2 y1)
                             (- z2 z1)))
             (magnitude (compute-magnitude delta)))
        (declare (dynamic-extent delta))
        (scale-vector delta (/ 1.0 magnitude))))))

@export
(defun distance-between (vec-a vec-b)
  "Compute the distance between VEC-A and VEC-B vectors."
  (declare (optimize (speed 3)))
  (if (and (typep vec-a 'vector2)
           (typep vec-b 'vector2))
      (locally (declare (vector2 vec-a vec-b))
        (with-accessors ((x1 x) (y1 y)) vec-a
          (with-accessors ((x2 x) (y2 y)) vec-b
            (declare (single-float x1 y1 x2 y2))
            (the single-float
                 (sqrt (+ (expt (- x1 x2) 2)
                          (expt (- y1 y2) 2)))))))
      (locally (declare (vector3 vec-a vec-b))
        (with-accessors ((x1 x) (y1 y) (z1 z)) vec-a
          (with-accessors ((x2 x) (y2 y) (z2 z)) vec-b
            (declare (single-float x1 y1 z1 x2 y2 z2))
            (the single-float
                 (sqrt (+ (expt (- x1 x2) 2)
                          (expt (- y1 y2) 2)
                          (expt (- z1 z2) 2)))))))))
