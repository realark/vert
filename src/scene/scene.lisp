;;;; Base class for game world and menus

(in-package :recurse.vert)

(defclass scene (event-publisher scheduler)
  ((camera :initarg :camera
           :initform (make-instance 'camera
                                    :width 100
                                    :height 100
                                    :screen-width 100
                                    :screen-height 100)
           :accessor camera)
   (scene-ticks :initform 0
                :reader scene-ticks
                :documentation "Amount of milliseconds passed in the this scene.
Will be incremented by the update timestep after every update frame.")
   (scene-overlays :initform (make-array 1 :adjustable T :fill-pointer 0))
   (scene-input :initform (make-array 4 :adjustable T :fill-pointer 0)
                :accessor scene-input
                :documentation "list of input-devices hooked up to the scene"))
  (:documentation "Generic scene class."))

(defmethod initialize-instance :after ((scene scene) &rest args)
  (declare (ignore args))
  (with-slots (timer-fn) scene
    (setf timer-fn
          (lambda () (scene-ticks scene)))))

(defun add-scene-input (scene input)
  "Hook up INPUT to SCENE"
  (declare (scene scene)
           (input-device input))
  ;; TODO: Fire an event
  (unless (find input (scene-input scene))
    (vector-push-extend input (scene-input scene))))

(defun remove-scene-input (scene input)
  "Remove INPUT from SCENE"
  (declare (scene scene)
           (input-device input))
  ;; TODO: Fire an event
  (when (find input (scene-input scene))
    (with-accessors ((scene-input scene-input)) scene
      (setf scene-input (delete input scene-input)))))

(defmethod update ((scene scene) (delta-t-ms real) (null null))
  (declare (ignore scene delta-t-ms null)))

(defmethod update :after ((scene scene) delta-t-ms context)
  (loop :for device :across (scene-input scene) :do
       (after-input-update device))
  ;; run scheduler then advance scene time
  (scheduler-run-callbacks scene)
  (incf (slot-value scene 'scene-ticks) delta-t-ms))

(defmethod render ((scene scene) update-percent (null null) rendering-context)
  (declare (ignore null))
  (render scene update-percent (slot-value scene 'camera) rendering-context))

(defmethod render ((scene scene) update-percent (camera simple-camera) rendering-context)
  (declare (ignore camera scene update-percent camera rendering-context)))

(defmethod load-resources ((scene scene) rendering-context)
  ;; no-op
  )
(defmethod release-resources ((scene scene))
  ;; no-op
  )
