;;;; dummy ai that moves back and forth

(in-package :recurse.vert)

(defclass back-and-forth (direction-tracker obb-touch-tracker kinematic-object)
  ((agent-acceleration :initarg :agent-acceleration
                       :type vector2
                       :initform (make-acceleration-vector-seconds :x 600 :y 0)))
  (:documentation "A simple agent that moves in a given direction.
                   It will reverse direction upon impact with another object."))

(defmethod initialize-instance :after ((agent back-and-forth) &rest args)
  (declare (ignore args))
  (if (> (vector-x (slot-value agent 'agent-acceleration)) 0)
      (push-direction agent :EAST)
      (push-direction agent :WEST))
  (if (> (vector-y (slot-value agent 'agent-acceleration)) 0)
      (push-direction agent :SOUTH)
      (push-direction agent :NORTH)))

(defmethod update-user :after ((agent back-and-forth) delta-t-ms context)
  (apply-vector agent (slot-value agent 'agent-acceleration)))

(defmethod collision :after ((agent back-and-forth) (object aabb))
  (with-slots ((direction agent-acceleration))
      agent
    (setf (x direction) (* -1.0 (x direction))
          (y direction) (* -1.0 (y direction)))
    (if (> (vector-x direction) 0)
        (push-direction agent :EAST)
        (push-direction agent :WEST))
    (if (> (vector-y direction) 0)
        (push-direction agent :SOUTH)
        (push-direction agent :NORTH))))
