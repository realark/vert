;;;; 2d camera. Bridge between the game world and the computer screen.

(in-package :recurse.vert)

;;;; Camera class and methods
(defclass simple-camera (aabb)
  ((width :initform 1.0
          :reader width
          :type world-dimension
          :documentation "Width of the camera in world units. Autocomputed from screen-width and zoom/PPU.")
   (height :initform 1.0
           :reader height
           :type world-dimension
           :documentation "Height of the camera in world units. Autocomputed from screen-height and zoom/PPU.")
   (screen-width :initarg :screen-width
                 :initform 100
                 :accessor screen-width
                 :type screen-unit
                 :documentation "Width of the display screen in pixels.")
   (screen-height :initarg :screen-height
                  :initform 100
                  :accessor screen-height
                  :type screen-unit
                  :documentation "Height of the display screen in pixels")
   (pixels-per-unit :initarg :pixels-per-unit
                    :initform 1
                    :accessor pixels-per-unit
                    :documentation "The number of pixels in one world unit.")
   (zoom :initarg :zoom
         :initform 1.0
         :accessor zoom
         :documentation "A slot to zoom the camera in or out.")
   (scale
    :reader scale
    :documentation "The scaling factor to transform world units into pixels. Implemented as PIXELS-PER-UNIT times ZOOM."))
  (:documentation "2d camera which can be moved and zoom in and out."))

(defevent camera-screen-resized ((camera simple-camera))
    "screen-width or screen-height of a camera has changed.")

(flet ((calculate-and-store-camera-width-height (camera)
         ;; Calculate and stores the world width and height of CAMERA in the WIDTH and HEIGHT slots.
         (with-accessors ((screen-w screen-width)
                          (screen-h screen-height)
                          (scale scale)
                          (ppu pixels-per-unit))
             camera
           (setf (slot-value camera 'width) (coerce (/ screen-w scale) 'world-dimension))
           (setf (slot-value camera 'height) (coerce (/ screen-h scale) 'world-dimension))
           (fire-event camera camera-screen-resized)
           (values))))

  (defmethod initialize-instance :after ((camera simple-camera) &rest args)
             (declare (ignore args))
             (with-accessors ((zoom zoom)) camera
               ;; running through setf specializers bypassed by :initform
               (setf zoom zoom) ;; recalculate scale
               (calculate-and-store-camera-width-height camera)))

  (defmethod (setf screen-width) :around (value (camera simple-camera))
             (call-next-method (coerce value 'screen-unit) camera))

  (defmethod (setf screen-width) :after (value (camera simple-camera))
             (calculate-and-store-camera-width-height camera))

  (defmethod (setf screen-height) :around (value (camera simple-camera))
             (call-next-method (coerce value 'screen-unit) camera))

  (defmethod (setf screen-height) :after (value (camera simple-camera))
             (calculate-and-store-camera-width-height camera))

  (defmethod (setf zoom) :around (new-zoom (camera simple-camera))
             (with-slots (scale pixels-per-unit) camera
               (unless (typep new-zoom 'single-float)
                 (setf new-zoom (coerce new-zoom 'single-float)))
               (call-next-method new-zoom camera)))

  (defmethod (setf zoom) :after (value (camera simple-camera))
             (setf (slot-value camera 'scale)
                   (coerce (* (zoom camera) (pixels-per-unit camera)) 'camera-scale))
             (calculate-and-store-camera-width-height camera))

  (defmethod (setf pixels-per-unit) :after (value (camera simple-camera))
             (setf (slot-value camera 'scale)
                   (coerce (* (zoom camera) (pixels-per-unit camera)) 'camera-scale))
             (calculate-and-store-camera-width-height camera)))

(defgeneric world-to-screen-cords (game-object camera update-percent)
  (:documentation "Return x-y screen coordinates of the world object.")
  (:method ((object game-object) (camera simple-camera) update-percent)
    (declare (optimize (speed 3))
             (sdl-rectangle-drawable object)
             (simple-camera camera))
    (multiple-value-bind (drawable-x drawable-y) (interpolate-position object update-percent)
      (%world-to-screen-cords drawable-x drawable-y camera update-percent))))

(defun %world-to-screen-cords (world-x world-y camera update-percent)
  (declare (optimize (speed 3))
           (world-position world-x world-y)
           (simple-camera camera))
  (with-accessors ((scale scale)
                   (camera-x x)
                   (camera-y y))
      camera
    (multiple-value-bind (camera-x camera-y) (interpolate-position camera update-percent)
      (declare (camera-scale scale)
               (world-position camera-x camera-y))
      (the (values screen-unit screen-unit)
           (values (ceiling (* scale (- world-x camera-x)))
                   (ceiling (* scale (- world-y camera-y))))))))

(defgeneric world-to-screen-dimensions (game-object camera)
  (:documentation "Return width/height screen dimensions for GAME-OBJECT")
  (:method ((game-object game-object) (camera simple-camera))
    (declare (optimize (speed 3)
                       (space 3)))
    (with-slots (scale) camera
      (with-slots ((world-width width) (world-height height)) game-object
        (declare (world-dimension world-width world-height)
                 (camera-scale scale))
        (the (values screen-unit screen-unit)
             (values (ceiling (* scale world-width))
                     (ceiling (* scale world-height))))))))

;;;; bounded-camera

(defclass bounded-camera (simple-camera)
  ((min-x :initarg :min-x
          :initform nil
          :accessor min-x
          :type integer
          :documentation "Min x value the camera will show.")
   (max-x :initarg :max-x
          :initform nil
          :accessor max-x
          :type integer
          :documentation "Max x value the camera will show.")
   (min-y :initarg :min-y
          :initform nil
          :accessor min-y
          :type integer
          :documentation "Min y value the camera will show.")
   (max-y :initarg :max-y
          :initform nil
          :accessor max-y
          :type integer
          :documentation "Max y value the camera will show.")
   (min-z :initarg :min-z
          :initform nil
          :accessor min-z
          :type integer
          :documentation "Min z value the camera will show.")
   (max-z :initarg :max-z
          :initform nil
          :accessor max-z
          :type integer
          :documentation "Max z value the camera will show."))
  (:documentation "A camera which will not move outside of a user-defined x-y-z boundary."))

(defmethod (setf x) (value (camera bounded-camera))
  (with-accessors ((min min-x) (max max-x) (w width)) camera
    (when (and min (< value min)) (setf value min))
    (when (and max (> value (- max w))) (setf value (- max w))))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf y) (value (camera bounded-camera))
  (with-accessors ((min min-y) (max max-y) (h height)) camera
    (when (and min (< value min)) (setf value min))
    (when (and max (> value (- max h))) (setf value (- max h))))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf z) (value (camera bounded-camera))
  (with-accessors ((min min-z) (max max-z)) camera
    (when (and min (< value min)) (setf value min))
    (when (and max (> value max) (setf value max))))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf zoom) (value (camera simple-camera))
  (flet ((calculate-min-zoom (camera)
           ;; Calculate the minimum value the camera can zoom out to
           ;; while staying within the min/max boundary.
           ;; If there is no min or max boundary, -1 is returned.
           (with-accessors ((min-x min-x) (max-x max-x)
                            (min-y min-y) (max-y max-y)
                            (ppu pixels-per-unit)
                            (w screen-width) (h screen-height))
               camera
             (max (if (and max-x min-x) (/ w ppu (- max-x min-x)) -1)
                  (if (and max-y min-y) (/ h ppu (- max-y min-y)) -1)))))

    (let ((camera-max-zoom 4)
          (camera-min-zoom (calculate-min-zoom camera)))
      (when (< value camera-min-zoom)
        (setf value camera-min-zoom))
      (when (> value camera-max-zoom)
        (setf value camera-max-zoom))
      (setf (slot-value camera 'zoom) (coerce value 'single-float)))))


;; when the screen width/height changes, rerun zoom check
;; to prevent camera from leaving boundary
(defmethod (setf screen-width) :around (value (camera bounded-camera))
  (let ((old (screen-width camera))
        (new (call-next-method value camera)))
    (unless (= old new)
      (setf (zoom camera) (zoom camera)))
    new))

(defmethod (setf screen-height) :around (value (camera bounded-camera))
  (let ((old (screen-height camera))
        (new (call-next-method value camera)))
    (unless (= old new)
      (setf (zoom camera) (zoom camera)))
    new))

;;;; target-tracking camera
(defclass target-tracking-camera (simple-camera)
  ((target :initarg :target
           :initform nil
           :accessor target
           :documentation "When bound to non-nil, the camera will center around this world object.")
   (target-max-offset :initform 100
                      :initarg :target-max-offset
                      :reader target-max-offset
                      :documentation "Allow the target to move within a box of this slot's length before moving the camera.")
   (target-center-x :initform 0
                    :accessor target-center-x
                    :documentation "X center of the screen based on the target")
   (target-center-y :initform 0
                    :accessor target-center-y
                    :documentation "Y center of the screen based on the target"))
  (:documentation "A camera which will track a given target"))

(defmethod initialize-instance :after ((camera target-tracking-camera) &rest args)
  (declare (ignore args))
  (with-accessors ((target target)) camera
    (when target
      (let ((old-target target))
        (setf target nil)
        (setf target old-target)))))

(defmethod (setf target) :before (new-target (camera target-tracking-camera))
  (unless (eq new-target (target camera))
    (when new-target
      (setf (target-center-x camera) (+  (x new-target) (/ (width new-target) 2)))
      (setf (target-center-y camera) (+  (y new-target) (/ (height new-target) 2)))
      (add-subscriber new-target camera object-moved))
    (when (target camera)
      (remove-subscriber (target camera) camera object-moved))))

(flet ((camera-track-target (camera)
         ;; Center the camera around its target.
         (with-accessors ((width screen-width) (height screen-height)
                          (center-x target-center-x) (center-y target-center-y)
                          (max-offset target-max-offset)
                          (scale scale) (target target))
             camera
           (when target
             (let ((max-offset (/ max-offset scale)))
               (with-accessors ((target-x x) (target-y y)) target
                 (when (<= center-y (- target-y max-offset))
                   (setf center-y (- target-y max-offset)))
                 (when (>= center-y (+ target-y max-offset))
                   (setf center-y (+ target-y max-offset)))
                 (when (<= center-x (- target-x max-offset))
                   (setf center-x (- target-x max-offset)))
                 (when (>= center-x (+ target-x max-offset))
                   (setf center-x (+ target-x max-offset)))))
             (setf (x camera) (- center-x (/ width scale 2)))
             (setf (y camera) (- center-y (/ height scale 2)))))))

  (defmethod (setf target) :after (new-target (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf screen-width) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf screen-height) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf zoom) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defevent-callback object-moved ((object game-object) (camera simple-camera))
    (when (eq object (target camera))
      (camera-track-target camera))))

;;;; auto-scaling camera

(defclass auto-scaling-camera (simple-camera)
  ((world-camera-width :initarg :world-camera-width
                       :initform (error ":world-camera-width required")
                       :accessor world-camera-width)
   (world-camera-height :initarg :world-camera-height
                       :initform (error ":world-camera-height required")
                       :accessor world-camera-height))
  (:documentation "A camera which automatically scales pixels-per-unit so the same world rectangle is always shown no matter the size of the display. "))

(labels ((update-pixels-per-unit (camera)
           (with-accessors ((screen-width screen-width)
                            (screen-height screen-height)
                            (world-width world-camera-width)
                            (world-height world-camera-height)
                            (ppu pixels-per-unit))
               camera
             (let* ((width-scale (/ screen-width world-width))
                    (height-scale (/ screen-height world-height)))
               ;; TODO: draw black bars when scaling is not 1:1 or enforce a specific resolution at startup.
               (setf ppu (min width-scale height-scale))))))

  (defmethod initialize-instance :after ((camera auto-scaling-camera) &rest args)
             (declare (ignore args))
             (update-pixels-per-unit camera))

  (defmethod (setf screen-width) :after (value (camera auto-scaling-camera))
             (update-pixels-per-unit camera))

  (defmethod (setf screen-height) :after (value (camera auto-scaling-camera))
             (update-pixels-per-unit camera)))

;;;; default camera

(defclass camera (auto-scaling-camera bounded-camera target-tracking-camera)
  ()
  (:documentation "The default camera class."))
