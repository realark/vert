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

(defmethod update :before ((scene platformer-game-scene) delta-t-ms (null null))
  (declare (ignore null))
  (with-accessors ((spatial-partition spatial-partition)
                   (gravity gravity-vector))
      scene
    (do-spatial-partition (game-object spatial-partition)
      (when (typep game-object 'kinematic-object)
        (apply-gravity game-object gravity))))
  (values))
