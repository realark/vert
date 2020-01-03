(in-package :recurse.vert)

@export-class
(defclass overlay (transform)
  ((width :initarg :width
          :accessor width
          :initform (error ":width required"))
   (height :initarg :height
           :accessor height
           :initform (error ":height required")))
  (:documentation "An OVERLAY scales itself to a camera and renders its child objects to the screen independent of a camera's world position."))

(defmethod render ((overlay overlay) update-percent camera rendering-context)
  (setf (width overlay) (width camera)
        (height overlay) (height camera)
        (parent overlay) camera)
  (loop :for child :across (transform-children overlay) :do
       (render child 1.0 camera rendering-context)))

(defmethod pre-update ((overlay overlay))
  (loop :for child :across (transform-children overlay) :do
       (pre-update child)))

(defmethod update :after ((overlay overlay) delta-t-ms world-context)
  (loop :for child :across (transform-children overlay) :do
       (update child delta-t-ms world-context)))

(defmethod load-resources :after ((overlay overlay) rendering-context)
  (loop :for child :across (transform-children overlay) :do
       (load-resources child rendering-context)))

(defmethod release-resources :after ((overlay overlay))
  (loop :for child :across (transform-children overlay) :do
       (release-resources child)))
