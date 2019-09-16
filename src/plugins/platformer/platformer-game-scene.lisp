(in-package :recurse.vert)

;;;; platformer-game-scene

(defparameter *default-gravity-acceleration-seconds* 500
  "The default downward force of gravity in px/s/s")

@export-class
(defclass platformer-game-scene (game-scene physics-context-2d)
  ((gravity-vector :initarg :gravity-vector
                   :initform (make-acceleration-vector-seconds :y *default-gravity-acceleration-seconds*)
                   :reader gravity-vector
                   :documentation "Force and direction of gravity on movable objects"))
  (:documentation "A 2d platformer map with gravity."))

@export
(defgeneric apply-gravity (kinematic-object vector2)
  (:documentation "Apply a 2d gravity vector to an object")
  (:method ((object kinematic-object) vector)
    (declare (vector2 vector))
    (apply-vector object vector)))

(defmethod found-object-to-update ((scene platformer-game-scene) (game-object kinematic-object))
  (with-accessors ((gravity gravity-vector)) scene
    (apply-gravity game-object gravity)))
