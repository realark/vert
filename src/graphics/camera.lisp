;;;; 2d camera. Bridge between the game world and the computer screen.

(in-package :recurse.vert)

;;;; Camera class and methods
@export-class
(defclass simple-camera (aabb)
  ((width :initform (error ":width required")
          :reader width
          :type world-dimension
          :documentation "Width of the camera in world units.")
   (height :initform (error ":height required")
           :reader height
           :type world-dimension
           :documentation "Height of the camera in world units.")
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
   (zoom :initarg :zoom
         :initform 1.0
         :accessor zoom
         :documentation "A slot to zoom the camera in or out.")
   (projection-matrix :initform nil
                      :documentation "World projection matrix. Autocomputed."))
  (:documentation "2d camera which can be moved and zoom in and out."))

(defevent camera-screen-resized ((camera simple-camera))
    "screen-width or screen-height of a camera has changed.")

(defun set-camera-projection-matrix (camera update-percent)
  (declare (simple-camera camera)
           ((single-float 0.0 1.0) update-percent))
  (with-slots ((screen-width screen-width)
               (screen-height screen-height)
               (world-width width)
               (world-height height)
               zoom
               projection-matrix)
      camera
    (multiple-value-bind (ix iy)
        (interpolate-position camera update-percent)
      (setf projection-matrix
            (n-ortho-matrix (or projection-matrix (sb-cga:identity-matrix))
                            ix
                            (float (+ ix (/ world-width zoom)))
                            (float (+ iy (/ world-height zoom)))
                            iy
                            100.0
                            -100.0)))))

(defmethod projection-matrix ((camera simple-camera))
  (with-slots (projection-matrix) camera
    (unless projection-matrix
      (set-camera-projection-matrix camera 1.0))
    projection-matrix))

(labels ((update-projection-matrix (camera)
           (fire-event camera camera-screen-resized)))

  (defmethod initialize-instance :after ((camera simple-camera) &rest args)
             (declare (ignore args))
             (with-accessors ((zoom zoom)) camera
               ;; running through setf specializers bypassed by :initform
               (setf zoom zoom) ;; coerce to float
               (with-slots (screen-width screen-height) camera
                 (multiple-value-bind (w h)
                     (window-size-pixels (application-window *engine-manager*))
                   (setf screen-width w
                         screen-height h)))
               (update-projection-matrix camera)))

  (defmethod (setf screen-width) :around (value (camera simple-camera))
             (prog1
                 (call-next-method (coerce value 'screen-unit) camera)
               (update-projection-matrix camera)))

  (defmethod (setf screen-height) :around (value (camera simple-camera))
             (prog1
                 (call-next-method (coerce value 'screen-unit) camera)
               (update-projection-matrix camera)))

  (defmethod (setf x) :after (new-val (camera simple-camera))
             (update-projection-matrix camera))

  (defmethod (setf y) :after (new-val (camera simple-camera))
             (update-projection-matrix camera))

  (defmethod (setf zoom) :around (new-zoom (camera simple-camera))
             (prog1
                 (call-next-method (coerce new-zoom 'single-float) camera)
               (update-projection-matrix camera))))

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
  (with-accessors ((min min-x) (max max-x) (w width) (z zoom)) camera
    (when (and min (< value min))
      (setf value min))
    (when (and max (> value (- max (/ w z))))
      (setf value (- max (/ w z)))))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf y) (value (camera bounded-camera))
  (with-accessors ((min min-y) (max max-y) (h height) (z zoom)) camera
    (when (and min (< value min))
      (setf value min))
    (when (and max (> value (- max (/ h z))))
      (setf value (- max (/ h z)))))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf z) (value (camera bounded-camera))
  (with-accessors ((min min-z) (max max-z)) camera
    (when (or min max)
      (error "z bounded camera not implemented")))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf zoom) :after (value (camera bounded-camera))
  (flet ((calculate-min-zoom (camera)
           ;; Calculate the minimum value the camera can zoom out to
           ;; while staying within the min/max boundary.
           ;; If there is no min or max boundary, -1 is returned.
           (with-accessors ((min-x min-x) (max-x max-x)
                            (min-y min-y) (max-y max-y)
                            (w width) (h height)
                            (screen-w screen-width) (screen-h screen-height))
               camera
             (max (if (and max-x min-x)
                      ;; cam-w / zoom == max-min-delta
                      (/ w (- max-x min-x))
                      -1)
                  (if (and max-y min-y)
                      (/ h (- max-y min-y))
                      -1)))))

    (let ((camera-max-zoom 4)
          (camera-min-zoom (calculate-min-zoom camera)))
      (when (< (zoom camera) camera-min-zoom)
        (setf (zoom camera) camera-min-zoom))
      (when (> (zoom camera) camera-max-zoom)
        (setf (zoom camera) camera-max-zoom))
      ;; run through min-max checkers for the new zoom
      (setf (x camera) (x camera)
            (y camera) (y camera)))))

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
      (setf (target-center-x camera) (+ (x new-target) (/ (width new-target) 2)))
      (setf (target-center-y camera) (+ (y new-target) (/ (height new-target) 2)))
      (add-subscriber new-target camera object-moved))
    (when (target camera)
      (remove-subscriber (target camera) camera object-moved))))

(flet ((camera-track-target (camera)
         ;; Center the camera around its target.
         (with-accessors ((camera-width width) (camera-height height)
                          (center-x target-center-x) (center-y target-center-y)
                          (max-offset target-max-offset)
                          ;; TODO: check this with zoom
                          (zoom zoom) (target target))
             camera
           (when target
             (with-accessors ((target-x x) (target-y y)) target
               (when (<= center-y (- target-y max-offset))
                 (setf center-y (- target-y max-offset)))
               (when (>= center-y (+ target-y max-offset))
                 (setf center-y (+ target-y max-offset)))
               (when (<= center-x (- target-x max-offset))
                 (setf center-x (- target-x max-offset)))
               (when (>= center-x (+ target-x max-offset))
                 (setf center-x (+ target-x max-offset))))
             ;; (setf (x camera) (- (+ (x target) (/ (width target) 2))
             ;;                     (/ camera-width zoom 2)))
             ;; (setf (y camera) (- (+ (y target) (/ (height target) 2))
             ;;                     (/ camera-height zoom 2)))
             (setf (x camera) (- center-x
                                 (/ camera-width zoom 2)))
             (setf (y camera) (- center-y
                                 (/ camera-height zoom 2)))))))

  (defmethod (setf target) :after (new-target (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf screen-width) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf screen-height) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf width) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf height) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defmethod (setf zoom) :after (value (camera target-tracking-camera))
             (camera-track-target camera))

  (defevent-callback object-moved ((object game-object) (camera simple-camera))
    (when (eq object (target camera))
      (camera-track-target camera))))

;;;; default camera

(defclass camera (bounded-camera target-tracking-camera)
  ()
  (:documentation "The default camera class."))
