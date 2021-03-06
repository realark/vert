(in-package :recurse.vert)

(progn
  (defclass pause-scene (scene)
    ((scene :initarg :scene
            :accessor scene
            :documentation "The underlying scene.")
     (release-underlying-scene :initform nil
                               :accessor release-underlying-scene)
     (remaining-steps
      :initform 0
      :documentation "Number of steps left in :stepping run-state before underlying scene will pause")
     (next-frame-step-ts
      :initform 0
      :documentation "TS of next frame step")
     (time-between-frame-steps-ms
      :initform 1000
      :documentation "Time delay between update frames in :stepping run-state."))
    (:documentation "A scene which renders on top of a non-updated (nested) scene.
Useful for debugging and pausing."))

  (export 'release-underlying-scene))

;; TODO: use this instead of the current accessor
@export
(defmethod pause-scene-underlying-scene ((scene pause-scene))
  (scene scene))

(defmethod (setf pause-scene-underlying-scene) (new-value (scene pause-scene))
  (setf (scene scene) new-value))

(defun step-scene (pause-scene &key (num-steps 1) (time-between-frames-ms 1000))
  "Update the underlying scene NUM-STEPS."
  (declare (pause-scene pause-scene))
  (with-slots (scene
               remaining-steps
               next-frame-step-ts
               time-between-frame-steps-ms)
      pause-scene
    ;; (when (and (= 0 remaining-steps) (> num-steps 0))
    ;;   (when (%get-audio-player)
    ;;     (setf (music-state (%get-audio-player)) :playing)))
    (setf remaining-steps (max 0 num-steps)
          time-between-frame-steps-ms time-between-frames-ms
          next-frame-step-ts (+ (ticks) time-between-frame-steps-ms))))

(defmethod update ((pause-scene pause-scene))
  (with-slots (scene
               remaining-steps
               next-frame-step-ts
               time-between-frame-steps-ms)
      pause-scene
    (when (and (> remaining-steps 0)
               (>= (ticks) next-frame-step-ts))
      (update scene)
      (setf next-frame-step-ts (+ (ticks) time-between-frame-steps-ms))
      (decf remaining-steps)
      ;; (when (= 0 remaining-steps)
      ;;   (when (%get-audio-player)
      ;;     (setf (music-state (%get-audio-player)) :paused)))
      )
    (call-next-method pause-scene)))

(defmethod render ((pause-scene pause-scene) update-percent camera rendering-context)
  (when (scene pause-scene )
    (render (slot-value pause-scene 'scene) 1.0 nil rendering-context))
  (call-next-method pause-scene update-percent camera rendering-context))

(defmethod release-resources :after ((pause-scene pause-scene ))
  (when (and (scene pause-scene) (release-underlying-scene pause-scene))
    (release-resources (scene pause-scene ))))
