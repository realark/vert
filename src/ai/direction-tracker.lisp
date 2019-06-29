;;;; Direction tracking agent

(in-package :recurse.vert)

(defclass direction-tracker (agent obb)
  ((facing :initarg :facing
           :initform '(:EAST)
           :reader facing))
  (:documentation "An agent that tracks the direction it is facing"))

(defgeneric push-direction (direction-tracker direction-keyword)
  (:documentation "Face DIRECTION-TRACKER in the DIRECTION-KEYWORLD.
Mutually exclusive dirs will be updated (e.g. pushing :east removes :west).")
  (:method ((direction-tracker direction-tracker) direction)
    (flet ((opposite-direction (direction)
             (case direction
               (:NORTH :SOUTH)
               (:SOUTH :NORTH)
               (:EAST :WEST)
               (:WEST :EAST)
               (otherwise nil))))
      (with-slots ((facing facing)) direction-tracker
        (unless (find direction facing)
          (let ((opposite (opposite-direction direction)))
            (when opposite
              ;; having an opposite means we were given a valid direction
              (setf facing
                    (sort
                     (append
                      (remove opposite facing)
                      (list direction))
                     #'string-lessp)))))))))

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
