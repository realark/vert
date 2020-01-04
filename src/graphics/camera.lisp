;;;; 2d camera. Bridge between the game world and the computer screen.

(in-package :recurse.vert)

;;;; Camera class and methods
@export-class
(defclass simple-camera (obb)
  ((screen-width :initarg :screen-width
                 :initform 100
                 :accessor screen-width
                 :type screen-unit
                 :documentation "Width of the display screen in pixels.")
   (screen-height :initarg :screen-height
                  :initform 100
                  :accessor screen-height
                  :type screen-unit
                  :documentation "Height of the display screen in pixels")
   (unzoomed-width :initform 0)
   (unzoomed-height :initform 0)
   (zoom :initarg :zoom
         :initform 1.0
         :accessor zoom
         :documentation "A slot to zoom the camera in or out.")
   (ortho-matrix :initform (identity-matrix))
   (ortho-interpolator :initform (make-matrix-interpolator)
                       :documentation "render interpolation for world projection matrix"))
  (:documentation "2d camera which can be moved and zoom in and out."))

(defmethod pre-update :before ((camera simple-camera))
  (with-slots (ortho-matrix ortho-interpolator) camera
    (interpolator-update ortho-interpolator
                         ortho-matrix))
  (values))

(defmethod update :after ((camera simple-camera) delta-t-ms world-context)
  (with-slots (ortho-matrix ortho-interpolator) camera
    (let ((tmp (ortho-matrix (x camera)
                             (+ (x camera) (width camera))
                             (+ (y camera) (height camera))
                             (y camera)
                             100.0
                             -100.0)))
      (declare (dynamic-extent tmp))
      (copy-array-contents tmp ortho-matrix)
      (values))))

(defun interpolated-world-projection-matrix (camera update-percent)
  "Get CAMERA's projection matrix in world space"
  (declare (simple-camera camera)
           ((single-float 0.0 1.0) update-percent))
  (with-slots (ortho-matrix ortho-interpolator) camera
    (interpolator-compute ortho-interpolator ortho-matrix update-percent)))

(defevent camera-screen-resized ((camera simple-camera))
    "screen-width or screen-height of a camera has changed.")

(defmethod initialize-instance :after ((camera simple-camera) &rest args)
  (declare (ignore args))
  (with-accessors ((zoom zoom)) camera
    (with-slots (unzoomed-width unzoomed-height) camera
      (setf unzoomed-width (width camera)
            unzoomed-height (height camera)
            ;; set zoom last so its accessor hook runs and resizes camera
            ;; according to zoom
            zoom zoom))
    (with-slots (screen-width screen-height) camera
      (multiple-value-bind (w h)
          (window-size-pixels (application-window *engine-manager*))
        (setf screen-width w
              screen-height h)))
    (fire-event camera camera-screen-resized)))

(defmethod (setf screen-width) :around (value (camera simple-camera))
  (prog1
      (call-next-method (coerce value 'screen-unit) camera)
    (fire-event camera camera-screen-resized)))

(defmethod (setf screen-height) :around (value (camera simple-camera))
  (prog1
      (call-next-method (coerce value 'screen-unit) camera)
    (fire-event camera camera-screen-resized)))

(defmethod (setf x) :after (new-val (camera simple-camera))
  (fire-event camera camera-screen-resized))

(defmethod (setf y) :after (new-val (camera simple-camera))
  (fire-event camera camera-screen-resized))

(defmethod (setf zoom) :around (new-zoom (camera simple-camera))
  (prog1
      (call-next-method (coerce new-zoom 'single-float) camera)
    (with-slots (unzoomed-width unzoomed-height zoom) camera
      (setf (width camera) (/ unzoomed-width zoom))
      (setf (height camera) (/ unzoomed-height zoom)))
    (fire-event camera camera-screen-resized)))

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
    (when (and min (< value min))
      (setf value min))
    (when (and max (> value (- max w)))
      (setf value (- max w))))
  (call-next-method (coerce value 'world-position) camera))

(defmethod (setf y) (value (camera bounded-camera))
  (with-accessors ((min min-y) (max max-y) (h height)) camera
    (when (and min (< value min))
      (setf value min))
    (when (and max (> value (- max h)))
      (setf value (- max h))))
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
   (destination :initform (vector2)
                :documentation "The XY destination the camera is moving towards"))
  (:documentation "A camera which will track a given target"))

(defmethod initialize-instance :after ((camera target-tracking-camera) &rest args)
  (declare (ignore args))
  (with-slots (destination) camera
    (setf (x destination) (x camera)
          (y destination) (y camera)))
  (with-accessors ((target target)) camera
    (when target
      (let ((old-target target))
        (setf target nil)
        (setf target old-target)))))

(flet ((camera-track-target (camera)
         ;; Center the camera around its target.
         (with-slots (target destination) camera
           (when target
             (setf (x destination) (- (+ (x target) (/ (width target) 2.0))
                                      (/ (width camera) 2.0))
                   (y destination) (- (+ (y target) (/ (height target) 2.0))
                                      (/ (height camera) 2.0)))
             #+nil
             (setf (x camera) (x destination)
                   (y camera) (y destination))))
         #+nil
         (with-accessors ((camera-width width) (camera-height height)
                          (center-x target-center-x) (center-y target-center-y)
                                        ; (max-offset target-max-offset)
                          (target target))
             camera
           (when target
             (let ((target-x (x target))
                   (max-offset 0) ; FIXME
                   (target-y (y target)))
               (when (<= center-y (- target-y max-offset))
                 (setf center-y (- target-y max-offset)))
               (when (>= center-y (+ target-y max-offset))
                 (setf center-y (+ target-y max-offset)))
               (when (<= center-x (- target-x max-offset))
                 (setf center-x (- target-x max-offset)))
               (when (>= center-x (+ target-x max-offset))
                 (setf center-x (+ target-x max-offset))))
             (let* ((new-camera-x (- center-x
                                     (/ camera-width 2)))
                    (new-camera-y (- center-y
                                     (/ camera-height 2))))
               (setf (x camera) new-camera-x
                     (y camera) new-camera-y))))))

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

  (defmethod (setf target) :before (new-target (camera target-tracking-camera))
             (unless (eq new-target (target camera))
               (when new-target
                 (add-subscriber new-target camera object-moved)
                 (camera-track-target camera))
               (when (target camera)
                 (remove-subscriber (target camera) camera object-moved))))

  (defevent-callback object-moved ((object game-object) (camera target-tracking-camera))
    (when (eq object (target camera))
      (camera-track-target camera))))

@export
(defun camera-snap-to-target (camera)
  "Instantly move CAMERA to its target."
  (declare (target-tracking-camera camera))
  (with-slots (destination) camera
    (setf (x camera) (x destination)
          (y camera) (y destination))
    (recycle camera)
    camera))

(defmethod update ((camera target-tracking-camera) delta-t-ms world-context)
  (declare (optimize (speed 3)))
  (flet ((camera-lerp (a b time)
           (declare (single-float a b time))
           (+ a
              (* (- b a)
                 time))))
    (declare (inline camera-lerp))
    (with-slots (destination) camera
      (let ((time 0.1))
        (setf (x camera) (camera-lerp (x camera) (x destination) time)
              (y camera) (camera-lerp (y camera) (y destination) time)))))
  (call-next-method camera delta-t-ms world-context))

;;;; default camera

(defclass camera (bounded-camera target-tracking-camera)
  ()
  (:documentation "The default camera class."))
