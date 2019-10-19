;;;; Direction tracking agent

(in-package :recurse.vert)

(defclass direction-tracker (agent obb)
  ((facing :initarg :facing
           :initform (list :EAST)
           :reader facing)
   (simultanious-directions-p :initarg :simultainous-directions-p
                              :initform t))
  (:documentation "An agent that tracks the direction it is facing"))

(defun push-direction (direction-tracker direction)
  "Face DIRECTION-TRACKER towards DIRECTION. Mutually exclusive dirs will be updated (e.g. pushing :east removes :west)."
  (declare (direction-tracker direction-tracker)
           (keyword direction))
  (flet ((opposite-direction (direction)
           (case direction
             (:NORTH :SOUTH)
             (:SOUTH :NORTH)
             (:EAST :WEST)
             (:WEST :EAST)
             (otherwise nil))))
    (with-slots (facing simultanious-directions-p) direction-tracker
      (unless (find direction facing)
        (let ((opposite (opposite-direction direction)))
          (if simultanious-directions-p
              (when opposite
                ;; having an opposite means we were given a valid direction
                (setf facing
                      (sort
                       (append
                        (remove opposite facing)
                        (list direction))
                       #'string-lessp)))
              ;; only one direction is allowed so just replace the first element
              (setf (first facing) direction)))))))

(defgeneric is-facing (agent point)
  (:documentation "T if the agent is facing the given point")
  (:method ((agent direction-tracker) object)
    (with-accessors ((facing facing)) agent
      (let ((delta-x (- (x agent) (x object)))
            (delta-y (- (y agent) (y object))))
        (or (and (find :NORTH facing) (<= 0 delta-y))
            (and (find :SOUTH facing) (>= 0 delta-y))
            (and (find :WEST facing) (<= 0 delta-x))
            (and (find :EAST facing) (>= 0 delta-x)))))))
