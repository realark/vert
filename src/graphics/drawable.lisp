(in-package :recurse.vert)

;;;; Base classes for drawing game-objects

(defclass drawable (transform)
  ((draw-component
    :initarg :draw-component ; will likely be overridden by subclass
    :initform (error ":draw-component required")
    :accessor draw-component
    :documentation "Determines the draw-component used to render the game-object")
   (color :initarg :color
          :initform nil
          :documentation "A color mod blended with the drawable. Nil has the same effect as *white*."
          :accessor color)
   (interpolator :initform (make-matrix-interpolator)
                 :documentation "Holds the previous position of the game-object. Used for interpolating between update frames."))
  (:documentation "A game-object which can be rendered to the screen. Actual rendering is done by DRAW-COMPONENT slot."))

(defmethod render ((drawable drawable) update-percent (camera simple-camera) (context gl-context))
  (with-slots (draw-component) drawable
    (render draw-component update-percent camera context)))

@inline
(defun sprite-transform (transform)
  "Construct a matrix which can be used to render TRANSFORM as a sprite."
  (declare (optimize (speed 3)))
  (let ((translate (translation-matrix (width transform)
                                       (height transform)
                                       0.0))
        (dimensions (scale-matrix (width transform)
                                  (height transform)
                                  1.0)))
    (declare (dynamic-extent translate dimensions))
    (matrix*
     (local-to-world-matrix transform)
     ;; render with upper-left = object's origin
     translate
     dimensions)))

(defmethod initialize-instance :after ((transform drawable) &rest args)
  (declare (optimize (speed 3))
           (ignore args))
  (with-slots (interpolator) transform
    (let ((m (sprite-transform transform)))
      (declare (dynamic-extent m))
      (interpolator-update interpolator m))))

(defmethod pre-update :before ((transform drawable))
  (declare (optimize (speed 3)))
  (with-slots (interpolator) transform
    (let ((m (sprite-transform transform)))
      (declare (dynamic-extent m))
      (interpolator-update interpolator m)))
  (values))

(defmethod recycle ((drawable drawable))
  ;; drop the previous matrix
  (pre-update drawable))

(defun interpolated-sprite-matrix (drawable update-percent)
  (declare (optimize (speed 3))
           ((single-float 0.0 1.0) update-percent))
  (with-slots (interpolator) drawable
    (unless (= update-percent
               (matrix-interpolator-cached-update-percent interpolator))
      (let ((m (sprite-transform drawable)))
        (declare (dynamic-extent m))
        (interpolator-compute interpolator
                              m
                              update-percent)))
    (matrix-interpolator-imatrix interpolator)))
