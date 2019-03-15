(in-package :recurse.vert)

(deftype parallax-factor ()
  "Parallax constant to apply to backgrounds.
Smaller numbers will scroll slower, larger number will scroll faster. 1 will scroll at an equal rate to the main layer (presumably the player)."
  `(single-float 0.0001  10000.0))

@export-class
(defclass parallax-image (static-sprite)
  ((horizontal-parallax :initarg :horizontal-parallax
                        :initform 1.0
                        :accessor horizontal-parallax
                        :documentation "x-axis scrolling factor.")
   (vertical-parallax :initarg :vertical-parallax
                      :initform 1.0
                      :accessor vertical-parallax
                      :documentation "y-axis scrolling factor")))

(defmethod world-to-screen-cords ((image parallax-image) (camera simple-camera) update-percent)
  (declare (optimize (speed 3)))
  (with-accessors ((horizontal-parallax horizontal-parallax)
                   (vertical-parallax vertical-parallax))
      image
    (multiple-value-bind (drawable-x drawable-y) (interpolate-position image update-percent)
      (with-accessors ((ppu pixels-per-unit)) camera
        (multiple-value-bind (camera-x camera-y) (interpolate-position camera update-percent)
          (declare (world-position drawable-x drawable-y camera-x camera-y)
                   (parallax-factor horizontal-parallax vertical-parallax)
                   ((integer 0 1000) ppu))
          (the (values screen-unit screen-unit)
               (values (round (* ppu (- drawable-x (* horizontal-parallax camera-x))))
                       (round (* ppu (- drawable-y (* vertical-parallax camera-y)))))))))))

(defmethod world-to-screen-dimensions ((image parallax-image) (camera simple-camera))
  (declare (optimize (speed 3)))
  (with-accessors ((ppu pixels-per-unit)) camera
    (with-slots ((world-width width)
                 (world-height height))
        image
      (declare (world-dimension world-width world-height)
               ((integer 0 1000) ppu))
      (the (values screen-unit screen-unit)
           (values (ceiling (* ppu world-width))
                   (ceiling (* ppu world-height)))))))

(defmethod world-to-wrapped-screen-dimensions ((image parallax-image) (camera simple-camera))
  (declare (optimize (speed 3)))
  (with-accessors ((ppu pixels-per-unit)) camera
    (with-slots ((world-width width)
                 (wrap-width wrap-width)
                 (world-height height)
                 (wrap-height wrap-height))
        image
      (declare (world-dimension world-width world-height)
               ((integer 1 1000) ppu))
      (the (values screen-unit screen-unit)
           (values (ceiling (* ppu (the world-dimension (or wrap-width world-width))))
                   (ceiling (* ppu (the world-dimension (or wrap-height world-height)))))))))

@export-class
(defclass scene-background (aabb)
  ((layers :initform nil)
   (wrap-width :initarg :wrap-width
               :initform nil
               :documentation "TODO")
   (orig-wrap-width :initform nil)
   (wrap-height :initarg :wrap-height
                :initform nil
                :documentation "TODO"))
  (:documentation "Image displayed behind a scene."))

(defmethod initialize-instance :after ((background scene-background) &key layers)
  (with-slots (wrap-width orig-wrap-width wrap-height) background
    (unless layers (error ":layers required"))
    (when wrap-width (setf orig-wrap-width wrap-width))
    (setf
     (slot-value background 'layers)
     (loop :with parallax-images = (list)
        :for item :in layers :do
          (setf parallax-images
                (nconc parallax-images
                       (list (cond
                               ((stringp item) (make-instance 'parallax-image
                                                              :wrap-width wrap-width
                                                              :wrap-height wrap-height
                                                              :width (width background)
                                                              :height (height background)
                                                              :path-to-image item))
                               (T item)))))

        :finally (return (make-array (length parallax-images) :initial-contents parallax-images))))))

(defmethod render ((background scene-background) update-percent camera rendering-context)
  (declare (optimize (speed 3)))
  (with-slots (layers wrap-width orig-wrap-width wrap-height) background
    (loop :for layer :across (the (simple-array parallax-image) layers) :do
         (render layer update-percent camera rendering-context))))

(defmethod update :after ((background scene-background) delta-t-ms world-context)
  (declare (optimize (speed 3)))
  (with-slots (layers wrap-width orig-wrap-width wrap-height) background
    (loop :for layer :across (the (simple-array parallax-image) layers) :do
         (update layer delta-t-ms world-context))))

(defmethod pre-update :after ((background scene-background))
  (declare (optimize (speed 3)))
  (with-slots (layers wrap-width orig-wrap-width wrap-height) background
    (loop :for layer :across (the (simple-array parallax-image) layers) :do
         (pre-update layer))))
