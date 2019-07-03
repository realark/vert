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

(defvar %hitbox-render-color% (make-color-rgba :r 255 :a 100))
(defvar %phantom-render-color% (make-color-rgba :b 255 :a 100))

(defmethod render :after ((game-object game-object) update-percent camera (renderer gl-context))
  (when (get-dev-config 'dev-mode-render-collision-hitboxes)
    (cond ((typep game-object 'phantom)
           (%render-polygon game-object %phantom-render-color% 1.0 camera renderer))
          ((not (eq game-object (hit-box game-object t)))
           (%render-polygon (hit-box game-object t) %hitbox-render-color% 1.0 camera renderer)))))


(defvar %triangle-shader% nil)
(defvar %triangle-vao% 0)
(defvar %triangle-vbo% 0)

(on-engine-start ('create-triangle-shader)
  (setf %triangle-shader% (%create-triangle-shader))
  (load-resources %triangle-shader% (rendering-context *engine-manager*))
  (let ((buffers (%create-triangle-vao)))
    (setf %triangle-vao% (first buffers))
    (setf %triangle-vbo% (second buffers))))

(on-engine-stop ('cleanup-triangle-shader)
  (release-resources %triangle-shader%)
  (gl:delete-vertex-arrays (list %triangle-vao%))
  (gl:delete-buffers (list %triangle-vbo%))
  (setf %triangle-vao% 0
        %triangle-vbo% 0
        %triangle-shader% nil))

(defun %render-polygon (game-object color update-percent camera renderer)
  (declare (optimize (speed 3)))
  ;; - bind the geometry shader,
  ;; - set color and position

  (gl-use-shader renderer %triangle-shader%)

  (if color
      (set-uniformf %triangle-shader%
                    "color"
                    (r color)
                    (g color)
                    (b color)
                    (a color))
      (set-uniformf %triangle-shader%
                    "color"
                    1.0 1.0 1.0 1.0))

  ;; set position, rotation, and size
  (let* ((sprite-transform (%sprite-transform game-object)))
    (declare (dynamic-extent sprite-transform))
    (set-uniform-matrix-4fv %triangle-shader%
                            "worldModel"
                            sprite-transform
                            nil))

  ;; set world projection
  (set-uniform-matrix-4fv %triangle-shader%
                          "worldProjection"
                          (interpolated-world-projection-matrix camera update-percent)
                          nil)

  (gl-use-vao renderer %triangle-vao%)
  ;; TODO loop and draw for arbitrary polygons
  (n-draw-arrays :triangle-fan 0 4)
  (values))

(defun %create-triangle-shader ()
  (let* ((vertex-shader-source
          "#version 330 core
layout (location = 0) in vec3 screenPos;

uniform mat4 worldModel;
uniform mat4 worldProjection;

void main()
{
  gl_Position = worldProjection * worldModel * vec4(screenPos, 1.0);
}")
         (fragment-shader-source
          "#version 330 core
out vec4 FragColor;

uniform vec4 color;

void main()
{
  FragColor = color;
}")
         (triangle-shader (make-instance 'shader
                                         :vertex-source vertex-shader-source
                                         :fragment-source fragment-shader-source)))
    triangle-shader))

(defun %create-triangle-vao ()
  (let ((vao 0)
        (vbo 0)
        (gl-vertices (alloc-gl-array  :float
                                      ;; positions
                                      ;; positions
                                      0.0   0.0  0.0
                                      0.0  -1.0  0.0
                                     -1.0  -1.0  0.0
                                     -1.0   0.0  0.0
                                     )))
    (unwind-protect
         (progn
           (setf vao (gl:gen-vertex-array)
                 vbo (gl:gen-buffer))

           (gl:bind-vertex-array vao)

           ;; put the vertices in the VBO
           (gl:bind-buffer :array-buffer vbo)
           (gl:buffer-data :array-buffer :static-draw gl-vertices)
           ;; position
           (gl:vertex-attrib-pointer 0 3 :float 0 (* 3 (cffi:foreign-type-size :float)) (* 0 (cffi:foreign-type-size :float)))
           (gl:enable-vertex-attrib-array 0)

           ;; note: this is okay because vertex-attrib-pointer binds the vertex shader's input
           (gl:bind-buffer :array-buffer 0)
           (gl:bind-vertex-array 0)
           (list vao vbo))
      (gl:free-gl-array gl-vertices))))
