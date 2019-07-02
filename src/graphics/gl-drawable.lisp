(in-package :recurse.vert)

(defclass gl-drawable (transform)
  ((shader :initform nil :reader shader)
   (vao :initform 0 :reader vao)
   (texture :initform nil :reader texture)
   (interpolator :initform (make-matrix-interpolator)))
  (:documentation "A class drawn with opengl.
Its reader slots will be used by the game scene to optimize rendering by reducing gl state changes."))

@inline
(defun %sprite-transform (transform)
  "Construct a matrix which can be used to render TRANSFORM as a sprite."
  (declare (optimize (speed 3)))
  (let ((translate (translation-matrix (* 1.0 (width transform))
                                       (* 1.0 (height transform))
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

(defmethod initialize-instance :after ((transform gl-drawable) &rest args)
  (declare (optimize (speed 3))
           (ignore args))
  (with-slots (interpolator) transform
    (let ((m (%sprite-transform transform)))
      (declare (dynamic-extent m))
      (interpolator-update interpolator m))))

(defmethod pre-update :before ((transform gl-drawable))
  (declare (optimize (speed 3)))
  (with-slots (interpolator) transform
    (let ((m (%sprite-transform transform)))
      (declare (dynamic-extent m))
      (interpolator-update interpolator m)))
  (values))

(defun interpolated-sprite-matrix (drawable update-percent)
  (declare (optimize (speed 3))
           ((single-float 0.0 1.0) update-percent))
  (with-slots (interpolator) drawable
    (unless (= update-percent
               (matrix-interpolator-cached-update-percent interpolator))
      (let ((m (%sprite-transform drawable)))
        (declare (dynamic-extent m))
        (interpolator-compute interpolator
                              m
                              update-percent)))
    (matrix-interpolator-imatrix interpolator)))

(defun gl-< (gl-drawable1 gl-drawable2)
  (declare (gl-drawable gl-drawable1 gl-drawable2))
  (with-accessors ((s1 shader)
                   (vao1 vao)
                   (t1 texture))
      gl-drawable1
    (with-accessors ((s2 shader)
                     (vao2 vao)
                     (t2 texture))
        gl-drawable2
      (cond
        ((< (if s1 (shader-program-id s1) 0)
            (if s2 (shader-program-id s2) 0))
         t)
        ((> (if s1 (shader-program-id s1) 0)
            (if s2 (shader-program-id s2) 0))
         nil)
        ((< vao1 vao2)
         t)
        ((> vao1 vao2)
         nil)
        ((< (if t1 (texture-id t1) 0)
            (if t2 (texture-id t2) 0))
         t)
        ((> (if t1 (texture-id t1) 0)
            (if t2 (texture-id t2) 0))
         nil)
        ;; gl drawables are equal
        (t t)))))
