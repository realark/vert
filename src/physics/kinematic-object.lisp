(in-package :recurse.vert)

;; world objects which can move

(defclass kinematic-object (game-object)
  ((velocity-x :initarg :velocity-x
               :initform 0.0
               :accessor velocity-x
               :type vector-dimension)
   (velocity-y :initarg :velocity-y
               :initform 0.0
               :accessor velocity-y
               :type vector-dimension)
   (acceleration-x :initarg :acceleration-x
                   :initform 0.0
                   :accessor acceleration-x
                   :type vector-dimension)
   (acceleration-y :initarg :acceleration-y
                   :initform 0.0
                   :accessor acceleration-y
                   :type vector-dimension))
  (:documentation "Game-Object with velocity and acceleration."))

(defmethod (setf velocity-x) :around (value (kinematic-object kinematic-object))
  (declare (real value))
  (call-next-method (coerce value 'vector-dimension) kinematic-object))

(defmethod (setf velocity-y) :around (value (kinematic-object kinematic-object))
  (declare (real value))
  (call-next-method (coerce value 'vector-dimension) kinematic-object))

(defmethod (setf acceleration-x) :around (value (kinematic-object kinematic-object))
  (declare (real value))
  (call-next-method (coerce value 'vector-dimension) kinematic-object))

(defmethod (setf acceleration-y) :around (value (kinematic-object kinematic-object))
  (declare (real value))
  (call-next-method (coerce value 'vector-dimension) kinematic-object))

(defgeneric apply-vector (kinematic-object vector2)
  (:documentation "Apply a 2d vector to an object's acceleration.")
  (:method ((object kinematic-object) vector)
    (declare (vector2 vector))
    (with-accessors ((v-x x) (v-y y)) vector
      (with-accessors ((acc-x acceleration-x) (acc-y acceleration-y)) object
        (incf acc-x v-x)
        (incf acc-y v-y)
        (values)))))

(defun moving-towards (kinematic-object point-or-object)
  "T if kinematic-object's velocity will move the object closer to point-or-object"
  (flet ((distance-between (x1 y1 x2 y2)
           (sqrt (+ (expt (- x1 x2) 2)
                    (expt (- y1 y2) 2)))))
    (< (distance-between (+ (x kinematic-object) (velocity-x kinematic-object))
                         (+ (y kinematic-object) (velocity-y kinematic-object))
                         (x point-or-object)
                         (y point-or-object))
       (distance-between (x kinematic-object)
                         (y kinematic-object)
                         (x point-or-object)
                         (y point-or-object)))))
