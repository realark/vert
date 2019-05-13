(in-package :recurse.vert)


@export-class
(defclass angular-object (game-object)
  ((rotation-velocity :initform 0.0
                      :accessor rotation-velocity
                      :documentation "Object's clockwise angular velocity in radians/second"
                      :type vector-dimension)
   (rotation-acceleration :initform 0.0
                          :accessor rotation-acceleration
                          :documentation "Object's clockwise angular acceleration in radians/second"
                          :type vector-dimension))
  (:documentation "Game-Object which rotates about its center."))

#+nil
(progn ; rotate about an arbitrary point
  (defmotion angular-motion ((object angular-object) delta-t-ms (physics-context physics-context-2d))
    (with-slots (rotation-velocity
                 rotation-acceleration
                 rotation-point)
        object
      (let* ((rotation-radius (distance-between (slot-value object 'world-position)
                                                rotation-point))
             (theta (acos (/ (- (x object) (x rotation-point)) rotation-radius)))
             (rad-per-ms (/ rotation-velocity 1000))
             (displacement-rad (* rad-per-ms delta-t-ms))
             (new-theta (mod (+ theta displacement-rad) tau)))
        (setf (x object) (+ (x rotation-point) (* rotation-radius (cos new-theta))))
        (setf (y object) (+ (y rotation-point) (* rotation-radius (sin new-theta))))))
    (values)))

(defmotion angular-motion ((object angular-object) delta-t-ms (physics-context physics-context-2d))
  (with-slots (rotation-velocity
               rotation-acceleration
               rotation-point)
      object
    ;; (format T "update x. Old=~A => New=~A~%"
    ;;         (x object)
    ;;         (+ (x rotation-point) (* rotation-radius (cos new-theta))))
    (incf (rotation object) (* (/ delta-t-ms 1000) rotation-velocity))
    (setf rotation-velocity
          (+ ; friction applied to previous velocity
           (* (expt (friction-x physics-context) delta-t-ms) rotation-velocity)
           (* rotation-acceleration delta-t-ms)))
    (setf rotation-acceleration 0))
  (values))
