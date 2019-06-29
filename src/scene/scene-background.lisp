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
   (unparallax-position :initform nil)
   (vertical-parallax :initarg :vertical-parallax
                      :initform 1.0
                      :accessor vertical-parallax
                      :documentation "y-axis scrolling factor")))

(defmethod initialize-instance :after ((sprite parallax-image) &rest args)
  (declare (ignore args))
  (setf (slot-value sprite 'unparallax-position)
        (vector3 (x sprite) (y sprite) (z sprite))))

(defmethod update :after ((sprite parallax-image) delta-t-ms (scene scene))
  ;; move parallax's x-y to account for parallax factor
  (let ((camera (camera scene)))
    (with-slots (unparallax-position horizontal-parallax vertical-parallax) sprite
      (with-accessors ((x x) (y y) (h height)) sprite
        (with-accessors ((camera-x x) (camera-y y) (camera-h height)) camera
          (setf x (- camera-x (* horizontal-parallax (- camera-x (x unparallax-position))))
                y (- camera-y (* vertical-parallax (- camera-y (y unparallax-position))))))))))

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
                                                              :path-to-sprite item))
                               (T item)))))

        :finally (return (make-array (length parallax-images)
                                     :element-type 'parallax-image
                                     :initial-contents parallax-images))))))

(defmethod render ((background scene-background) update-percent camera rendering-context)
  (declare (optimize (speed 3)))
  (with-slots (layers wrap-width orig-wrap-width wrap-height) background
    (loop :for layer :across (the (simple-array parallax-image) layers) :do
         (render layer update-percent camera rendering-context))))

(defmethod load-resources ((background scene-background) rendering-context)
  (declare (optimize (speed 3)))
  (with-slots (layers wrap-width orig-wrap-width wrap-height) background
    (loop :for layer :across (the (simple-array parallax-image) layers) :do
         (load-resources layer rendering-context))))

(defmethod release-resources ((background scene-background))
    (with-slots (layers wrap-width orig-wrap-width wrap-height) background
      (loop :for layer :across (the (simple-array parallax-image) layers) :do
           (release-resources layer))))

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
