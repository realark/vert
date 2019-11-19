(in-package :recurse.vert)

@export
(defclass kinematic-object (game-object)
  ((velocity :initform (vector2))
   (acceleration :initform (vector2)))
  (:documentation "Game-Object with velocity and acceleration."))

@export
(defmethod velocity-x ((kinematic-object kinematic-object))
  (x (slot-value kinematic-object 'velocity)))

(defmethod (setf velocity-x) (value (kinematic-object kinematic-object))
  (setf (x (slot-value kinematic-object 'velocity)) (coerce value 'single-float)))

@export
(defmethod velocity-y ((kinematic-object kinematic-object))
  (y (slot-value kinematic-object 'velocity)))

(defmethod (setf velocity-y) (value (kinematic-object kinematic-object))
  (setf (y (slot-value kinematic-object 'velocity)) (coerce value 'single-float)))

@export
(defmethod acceleration-x ((kinematic-object kinematic-object))
  (x (slot-value kinematic-object 'acceleration)))

(defmethod (setf acceleration-x) (value (kinematic-object kinematic-object))
  (setf (x (slot-value kinematic-object 'acceleration)) (coerce value 'single-float)))

@export
(defmethod acceleration-y ((kinematic-object kinematic-object))
  (y (slot-value kinematic-object 'acceleration)))

(defmethod (setf acceleration-y) (value (kinematic-object kinematic-object))
  (setf (y (slot-value kinematic-object 'acceleration)) (coerce value 'single-float)))

@export
(defgeneric apply-vector (kinematic-object vector2)
  (:documentation "Apply a 2d vector to an object's acceleration.")
  (:method ((object kinematic-object) vector)
    (declare (optimize (speed 3))
             (vector2 vector))
    (with-accessors ((vec-x x) (vec-y y)) vector
      (with-accessors ((acc-x acceleration-x) (acc-y acceleration-y)) object
        (declare (single-float vec-x vec-y acc-x acc-y))
        (setf acc-x (+ acc-x vec-x)
              acc-y (+ acc-y vec-y))
        (values)))))
