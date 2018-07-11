;;;; Base class for game world and menus

(in-package :recurse.vert)

(defclass scene ()
  ((camera :initarg :camera
           :initform (make-instance 'camera
                                    :width 100
                                    :height 100
                                    :screen-width 100
                                    :screen-height 100)
           :accessor camera)
   (scene-input :initform (make-array 4 :adjustable T :fill-pointer 0)
                :reader scene-input
                :documentation "list of input-devices hooked up to the scene"))
  (:documentation "Generic scene class."))

(defgeneric add-scene-input (scene input)
  (:documentation "Hook up INPUT to SCENE")
  (:method ((scene scene) (input input-device))
    (unless (find input (scene-input scene))
      (vector-push-extend input (scene-input scene)))
    input))

(defmethod update ((scene scene) (delta-t-ms real) (null null))
  (declare (ignore scene delta-t-ms null)))

(defmethod render ((scene scene) (fraction-between-update real) (null null) rendering-context)
  (declare (ignore null))
  (render scene fraction-between-update (slot-value scene 'camera) rendering-context))

(defmethod render ((scene scene) (fraction-between-update real) (camera simple-camera) rendering-context)
  (declare (ignore camera scene fraction-between-update camera rendering-context)))

(defmethod load-resources ((scene scene) rendering-context)
  ;; no-op
  )
(defmethod release-resources ((scene scene))
  ;; no-op
  )

(defmethod update :after ((scene scene) delta-t-ms context)
  (declare (ignore delta-t-ms context))
  (loop for device across (scene-input scene) do
       (after-input-update device)))
