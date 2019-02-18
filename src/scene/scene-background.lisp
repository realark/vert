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

(defmethod world-to-screen-cords ((background parallax-image) (camera simple-camera) update-percent)
  (declare (optimize (speed 3))
           (sdl-rectangle-drawable background)
           (simple-camera camera))
  (with-accessors ((horizontal-parallax horizontal-parallax)
                   (vertical-parallax vertical-parallax))
      background
    (multiple-value-bind (drawable-x drawable-y) (interpolate-position background update-percent)
      (with-accessors ((scale scale)) camera
        (multiple-value-bind (camera-x camera-y) (interpolate-position camera update-percent)
          (declare (world-position drawable-x drawable-y camera-x camera-y)
                   (parallax-factor horizontal-parallax vertical-parallax)
                   (camera-scale scale))
          (the (values screen-unit screen-unit)
               (values (ceiling (* scale (- drawable-x (* horizontal-parallax camera-x))))
                       (ceiling (* scale (- drawable-y (* vertical-parallax camera-y)))))))))))

@export-class
(defclass scene-background (aabb)
  ((layers :initform nil)
   (wrap-width :initarg :wrap-width
               :initform nil
               :documentation "TODO")
   (orig-wrap-width :initform nil)
   (wrap-height :initarg :wrap-height
                :initform nil
                :documentation "TODO")
   (fixed-height :initform nil
                 :initarg :fixed-height
                 :documentation "When T, the y axis of the background will not scroll."))
  (:documentation "Image displayed behind a scene."))

(defmethod initialize-instance :after ((background scene-background) &key layers)
  (with-slots (wrap-width orig-wrap-width wrap-height) background
    (unless layers (error ":layers required"))
    (when wrap-width (setf orig-wrap-width wrap-width))
    (setf
     (slot-value background 'layers)
     (loop :with parallax-images = (make-array (length layers) :fill-pointer 0 :adjustable T)
        :for item :in layers :do
          (cond
            ((stringp item) (vector-push-extend (make-instance 'parallax-image
                                                               :wrap-width wrap-width
                                                               :wrap-height wrap-height
                                                               :width (width background)
                                                               :height (height background)
                                                               :path-to-image item)
                                                parallax-images))
            (T (vector-push-extend item parallax-images)))
        :finally (return parallax-images)))))

(defmethod render ((background scene-background) update-percent camera rendering-context)
  (declare (optimize (speed 3))
           (scene-background background)
           (simple-camera camera))
  (with-slots (layers fixed-height wrap-width orig-wrap-width wrap-height) background
    (loop :for layer :across layers :do
         (when fixed-height
           (when wrap-width
             (setf (slot-value layer 'wrap-width) (/ orig-wrap-width (scale camera))))
           (setf (width layer) (* 10 (the world-dimension (width camera)))
                 (height layer) (height camera)
                 (x layer) (if wrap-width 0.0 (x camera))
                 (y layer) (y camera)))
         (render layer 1.0 camera rendering-context))))

;; TODO: apply this logic to parallax-image
#+nil
(defmethod world-to-screen-cords ((background scene-background) (camera simple-camera))
  (declare (optimize (speed 3)
                     (space 3))
           (sdl-rectangle-drawable background)
           (simple-camera camera))
  (with-accessors ((drawable-x x)
                   (drawable-y y)
                   (horizontal-parallax horizontal-parallax)
                   (vertical-parallax vertical-parallax))
      background
    (with-accessors ((scale scale) (camera-x x) (camera-y y)) camera
      (declare (world-position drawable-x drawable-y camera-x camera-y)
               (parallax-factor horizontal-parallax vertical-parallax)
               (camera-scale scale))
      (the (values screen-unit screen-unit)
           (values (ceiling (* scale (- drawable-x (* horizontal-parallax camera-x))))
                   (ceiling (* scale (- drawable-y (* vertical-parallax camera-y)))))))))
