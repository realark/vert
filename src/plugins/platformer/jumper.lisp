;;;; Jump Agent

(in-package :recurse.vert)

(defclass jumper (agent obb-touch-tracker kinematic-object)
  ((max-jumps :initarg :max-jumps :initform 1 :reader max-jumps
              :documentation "Max jumps which can be preformed without touching the ground.")
   (jump-velocity :initarg :jump-velocity :initform 400
                  :reader jump-velocity
                  :documentation "TODO")
   (jump-height :initarg :jump-height :initform 120
                :reader jump-height
                :documentation "Max height of a single jump")
   (jumps :initform 0 :accessor jumps
          :documentation "Jumps the agent has currently used.")
   (jump-start-ts :initform nil :accessor jump-start-ts
                  :documentation "ms timestamp when the last jump started"))
  (:documentation "An agent which can jump."))

(defmethod is-jumping ((agent jumper))
  (< (velocity-y agent) (- *movement-threshold*)))

(defmethod is-falling ((agent jumper))
  (> (velocity-y agent) *movement-threshold*))

(defgeneric on-jump (jumper)
  (:documentation "Called when jumper jumps. Subclass can override or wrap."))
(defmethod on-jump ((jumper jumper)))
;; TODO Document and standardize units better
;; TODO Constructor fix jump-accel or veloc

(defgeneric is-touching-ground (jumper)
  (:documentation "Non-nil if JUMPER is on the ground")
  (:method ((jumper jumper))
    (objects-touching jumper :south)))

(defmethod jump ((jumper jumper))
  (with-accessors ((jumps jumps) (max max-jumps)
                   (jump-start-ts jump-start-ts)
                   (jump-height jump-height)
                   (jump-v jump-velocity))
      jumper
    (when (is-touching-ground jumper)
      (setf jumps max)
      (setf jump-start-ts nil))
    (if jump-start-ts
        (let* ((delta-t-ms (- (ticks) jump-start-ts))
               (final-displacement (+ (* (/ jump-v 1000) delta-t-ms)
                                      (max 0
                                           (- (* (/ jump-v 1000) delta-t-ms)
                                              (* .5 (/ *default-gravity-acceleration-seconds* (expt 1000 2)) (expt delta-t-ms 2)))))))
          (when (< final-displacement jump-height)
            ;; continue current jump
            (setf (velocity-y jumper) (- (/ jump-v 1000)))))
        (when (> jumps 0) ;; starting a new jump
          (on-jump jumper)
          (incf jumps -1)
          (setf jump-start-ts (ticks))
          ;; Modifying velocity instead of accel makes for a smoother looking jump.
          ;; Also allows human controlled jumpers to go higher by holding jump button.
          (setf (velocity-y jumper) (- (/ jump-v 1000)))))))
