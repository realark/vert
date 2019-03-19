;;;; Jump Agent

(in-package :recurse.vert)

@export-class
(defclass jumper (agent obb-touch-tracker stateful kinematic-object)
  ((max-jumps :initarg :max-jumps :initform 1 :reader max-jumps
              :documentation "Max jumps which can be preformed without touching the ground.")
   (normal-gravity-seconds :initarg :normal-gravity-seconds
                           :initform *default-gravity-acceleration-seconds*
                           :documentation "Assumed gravity (units/second/second) for computing short and long jump heights.")
   (long-jump-height :initarg :long-jump-height
                     :initform 25
                     :documentation "Number of units to move vertically for a jump in normal gravity.")
   (num-jumps-available :initform 1 :reader num-jumps-available)
   (target-y :initform nil)
   (tmp-jump-boost :initform nil
                   :accessor tmp-jump-boost
                   :documentation "Modify the long jump height for only the next jump.")
   (coyote-time-ms :initform 50
                   :initarg :coyote-time-ms
                   :documentation "meep-meep. Let jumpers hover in the air a short while after walking off a ledge (makes controls feel more responsive)."))
  (:documentation "An agent which can jump."))

(export 'num-jumps-available)
(export 'tmp-jump-boost)

(defmethod initialize-instance :after ((jumper jumper) &rest args)
  (declare (ignore args))
  (with-slots (num-jumps-available max-jumps) jumper
    (setf num-jumps-available max-jumps))
  ;; manages jump acceleration for jumper
  ;; TODO: Assuming normal gravity
  (add-state-machine
   jumper
   (make-state-machine (:name :jump-state :initial-state :on-ground)
     (:on-ground
      (:on-activate (:game-object jumper)
                    (with-slots (num-jumps-available max-jumps) jumper
                        (setf num-jumps-available max-jumps))))
     (:coyote
      (:on-activate (:game-object jumper :world-context scene)
                    (schedule scene
                              (+ (scene-ticks scene) (slot-value jumper 'coyote-time-ms))
                              (lambda ()
                                (when (eq :coyote (current-state-for jumper :jump-state))
                                  ;; only update state if no jumping has occurred
                                  (if (%is-touching-ground jumper)
                                      (change-state jumper scene :jump-state :on-ground)
                                      (change-state jumper scene :jump-state :falling)))))))
     (:long-jumping
      (:on-activate (:game-object jumper)
                    (with-slots (num-jumps-available target-y tmp-jump-boost long-jump-height) jumper
                      (setf target-y (- (y jumper) (or tmp-jump-boost long-jump-height)))))
      (:while-active (:game-object jumper :world-context scene)
                     (let ((distance-to-target (- (y jumper) (slot-value jumper 'target-y))))
                       (if (<= distance-to-target 0)
                           (when (>= (velocity-y jumper) 0)
                             (change-state jumper scene :jump-state :falling))
                           (%apply-jump-forces jumper))))
      (:on-deactivate (:game-object jumper :world-context scene)
                      (setf (tmp-jump-boost jumper) nil)))
     (:falling
      (:on-activate (:game-object jumper)
                    (with-slots (num-jumps-available target-y) jumper
                      (setf num-jumps-available 0)))
      (:while-active (:game-object jumper :world-context scene)
                     (when (%is-touching-ground jumper)
                       (change-state jumper scene :jump-state :on-ground))))))
  (add-state-machine
   jumper
   (make-state-machine (:name :jump-command :initial-state :not-sending-jump-command)
     (:sending-jump-command)
     (:not-sending-jump-command
      (:on-activate (:game-object jumper :world-context scene)
                    (setf (acceleration-y jumper) (max 0 (acceleration-y jumper)))
                    (change-state jumper scene :jump-state :falling))))))

@inline
(defun %apply-jump-forces (jumper)
  (declare (jumper jumper))
  (with-slots (tmp-jump-boost long-jump-height target-y normal-gravity-seconds) jumper
    ;; keep moving player up towards the jump-target
    (let* ((time-to-jump (* 0.75 (sqrt (/ (* 2 (or tmp-jump-boost long-jump-height))
                                          (/ normal-gravity-seconds (expt 1000.0 2))))))
           ;; time to jump = 75% of time to fall
           (velocity-to-reach-jump (/ (or tmp-jump-boost long-jump-height) time-to-jump)))
      (setf (velocity-y jumper) (min 0 (- velocity-to-reach-jump))))))

(defmethod update-user :after ((jumper jumper) delta-t-ms scene)
  (flet ((has-walked-off-ledge? (jumper)
              (and (not (%is-touching-ground jumper))
                   (>= (velocity-y jumper) 0)
                   (not (eq :falling (current-state-for jumper :jump-state))))))
    (cond ((and (not (eq :coyote (current-state-for jumper :jump-state)))
                (has-walked-off-ledge? jumper))
           (change-state jumper scene :jump-state :coyote))
          ((and (%is-touching-ceiling jumper) (not (%is-touching-ground jumper)))
           (change-state jumper scene :jump-state :falling)))))

@inline
(defun %is-touching-ground (jumper)
  (declare (jumper jumper))
  (objects-touching jumper :south))

@inline
(defun %is-touching-ceiling (jumper)
  (declare (jumper jumper))
  (objects-touching jumper :north))
