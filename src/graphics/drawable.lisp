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
          :documentation "A color mod blended (multiplied) with the drawable. Nil has the same effect as *white*."
          :accessor color)
   (interpolator0-dirty-p :initform t)
   (interpolator1-dirty-p :initform t)
   (interpolator :initform (make-matrix-interpolator)
                 :documentation "Holds the previous position of the game-object. Used for interpolating between update frames."))
  (:documentation "A game-object which can be rendered to the screen. Actual rendering is done by DRAW-COMPONENT slot."))

(defmethod render ((drawable drawable) update-percent (camera simple-camera) (context gl-context))
  (with-slots (draw-component) drawable
    (render draw-component update-percent camera context)))

@inline
(defun obb-render-transform (obb)
  "Construct a rendering transform for OBB."
  (declare (optimize (speed 3))
           (obb obb))
  (let ((translate (translation-matrix (width obb)
                                       (height obb)
                                       0.0))
        (dimensions (scale-matrix (width obb)
                                  (height obb)
                                  1.0)))
    (declare (dynamic-extent translate dimensions))
    (matrix*
     (local-to-world-matrix obb)
     ;; render with upper-left = object's origin
     translate
     dimensions)))

(defmethod initialize-instance :after ((transform drawable) &rest args)
  (declare (optimize (speed 3))
           (ignore args))
  (with-slots (interpolator) transform
    (let ((m (obb-render-transform transform)))
      (declare (dynamic-extent m))
      (interpolator-update interpolator m))))

(defmethod mark-obb-dirty :after ((drawable drawable))
  (setf (slot-value drawable 'interpolator0-dirty-p) t
        (slot-value drawable 'interpolator1-dirty-p) t))

(defmethod pre-update :before ((drawable drawable))
  (declare (optimize (speed 3)))
  (with-slots ((1dirty-p interpolator0-dirty-p)
               (0dirty-p interpolator1-dirty-p))
      drawable
    (when (or 1dirty-p 0dirty-p)
      (with-slots (interpolator) drawable
        (let ((m (obb-render-transform drawable)))
          (declare (dynamic-extent m))
          (interpolator-update interpolator m)))
      ;; it takes two passes to clean ourselves
      (if 0dirty-p
          ;; pass 1
          (setf 0dirty-p nil)
          ;; pass 2
          (setf 1dirty-p nil))))
  (values))

(defmethod recycle ((drawable drawable))
  ;; drop the previous matrix
  (pre-update drawable))

(defun interpolated-obb-matrix (drawable update-percent)
  (declare (optimize (speed 3))
           ((single-float 0.0 1.0) update-percent))
  (with-slots (interpolator) drawable
    (unless (= update-percent
               (matrix-interpolator-cached-update-percent interpolator))
      (let ((m (obb-render-transform drawable)))
        (declare (dynamic-extent m))
        (interpolator-compute interpolator
                              m
                              update-percent)))
    (matrix-interpolator-imatrix interpolator)))
