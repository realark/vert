(in-package :recurse.vert)

(defclass pause-scene (scene)
  ((scene :initarg :scene
          :accessor scene
          :documentation "The underlying scene.")
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

(defun step-scene (pause-scene &key (num-steps 1) (time-between-frames-ms 1000))
  "Update the underlying scene NUM-STEPS."
  (declare (pause-scene pause-scene))
  (with-slots (scene
               remaining-steps
               next-frame-step-ts
               time-between-frame-steps-ms)
      pause-scene
    (when (and (= 0 remaining-steps) (> num-steps 0))
      (when (%get-audio-player)
        (setf (music-state (%get-audio-player)) :playing)))
    (setf remaining-steps (max 0 num-steps)
          time-between-frame-steps-ms time-between-frames-ms
          next-frame-step-ts (+ (ticks) time-between-frame-steps-ms))))

(defmethod update ((pause-scene pause-scene) delta-t-ms world-context)
  (with-slots (scene
               remaining-steps
               next-frame-step-ts
               time-between-frame-steps-ms)
      pause-scene
    (when (and (> remaining-steps 0)
               (>= (ticks) next-frame-step-ts))
      (update scene delta-t-ms world-context)
      (setf next-frame-step-ts (+ (ticks) time-between-frame-steps-ms))
      (decf remaining-steps)
      (when (= 0 remaining-steps)
        (when (%get-audio-player)
          (setf (music-state (%get-audio-player)) :paused))))
    (call-next-method pause-scene delta-t-ms world-context)))

(defmethod render ((pause-scene pause-scene) update-percent camera rendering-context)
  (render (slot-value pause-scene 'scene) 0.0 nil rendering-context)
  (call-next-method pause-scene update-percent camera rendering-context))

(defmethod release-resources :after ((pause-scene pause-scene ))
  (when (scene pause-scene )
    (release-resources (scene pause-scene ))))
