(in-package :recurse.vert)

(defvar %sprite-key% (make-instance 'standard-object))

(progn
  (defstruct (sprite-source (:constructor %make-sprite-source))
    "Source rectangle for a sprite. w and h may be nil to specify 100%."
    (x 0 :type (integer 0 *))
    (y 0 :type (integer 0 *))
    (w nil :type (or null (integer 1 *)))
    (h nil :type (or null (integer 1 *))))

  (defun make-sprite-source (x y w h)
    (%make-sprite-source :x x :y y :w w :h h))

  (export '(make-sprite-source
            sprite-source-x
            sprite-source-y
            sprite-source-w
            sprite-source-h)))

(defvar %default-sprite-source%
  (make-sprite-source 0 0 nil nil))

(progn
  (defstruct color-map
    "A COLOR-MAP can be applied to a sprite to convert all matching FROM-COLORs to TO-COLOR in the sprite within the TOLERANCE range."
    (from-color *white* :type color)
    (to-color *white* :type color)
    (tolerance (/ 3.0 255.0) :type (single-float 0.0 1.0)))
  (export '(color-map make-color-map)))

(defvar %no-op-color-map%
  (make-color-map :from-color *white*
                  :to-color *white*
                  :tolerance 0.0))

(progn
  (defclass gl-sprite (game-object gl-drawable)
    ((path-to-sprite :initarg :path-to-sprite
                     :accessor path-to-sprite
                     :initform (error ":path-to-sprite required"))
     (sprite-source :initform nil
                    :initarg :sprite-source
                    :documentation "Rectangle subset of the sprite to render.
Nil to render the entire sprite."
                    :accessor sprite-source)
     (color :initarg :color
            :initform nil
            :documentation "A color mod blended with the original sprite.
Nil has the same effect as *white*."
            :accessor color)
     (color-maps :initform nil)
     (flip-vector :initform
                  (make-array 2
                              :initial-contents '(1.0 1.0)
                              :element-type 'single-float)
                  :reader flip-vector)
     (shader-cache :initarg :shader-cache
                   :initform *shader-cache*)
     (shader :initform nil :reader shader)
     (texture-cache :initarg :texture-cache
                    :initform
                    (getcache-default "texture-cache"
                                      *engine-caches*
                                      (make-instance 'resource-cache)))
     (texture :initform nil :reader texture)
     (buffer-cache :initarg :buffer-cache
                   :initform
                   (getcache-default "sprite-buffer-cache"
                                     *engine-caches*
                                     (make-instance 'counting-cache
                                                    :on-evict
                                                    (lambda (sprite-key gl-buffers)
                                                      (declare (ignore sprite-key))
                                                      (destructuring-bind (cached-vao cached-vbo) gl-buffers
                                                        (gl:delete-vertex-arrays (list cached-vao))
                                                        (gl:delete-buffers (list cached-vbo)))))))
     (vao :initform 0 :reader vao)
     (vbo :initform 0)
     (world-model :initform (sb-cga:identity-matrix)))
    (:documentation "A game-object rendered using opengl."))
  (export '(path-to-sprite
            sprite-source
            color)))

@export
(defun add-color-map (gl-sprite color-map)
  "Apply COLOR-MAP to GL-SPRITE. See doc for color-map struct for details."
  (declare (gl-sprite gl-sprite)
           (color-map color-map))
  (with-slots (color-maps) gl-sprite
    (if color-maps
        (error "multiple color maps not yet supported")
        (setf color-maps (make-array 1
                                     :element-type 'color-map
                                     :initial-contents (list color-map)
                                     :adjustable t)))))

(let ((translation-matrix (sb-cga:identity-matrix))
      (rotation-matrix (sb-cga:identity-matrix))
      (scale-matrix (sb-cga:identity-matrix)))
  (labels ((update-world-model (gl-sprite update-percent)
             "Update the sprites world-model matrix if its position has changed or if FORCE is non-nil."
             (declare (optimize (speed 3))
                      ((single-float 0.0 1.0) update-percent))
             ;; TODO: most objects will not change their model matrix. Could optimize to only compute on change.
             (multiple-value-bind (ix iy iz)
                 (interpolate-position gl-sprite update-percent)
               (declare (world-position ix iy iz))
               ;; Don't recompute if position, rotation, and size have not changed
               (n-matrix*
                (slot-value gl-sprite 'world-model)
                ;; add width and height to render from upper-left corner
                (n-translate* translation-matrix
                              (+ ix (width gl-sprite))
                              (+ iy (height gl-sprite))
                              iz)
                (n-rotate* rotation-matrix 0f0 0f0 (rotation gl-sprite))
                (n-scale* scale-matrix
                          (width gl-sprite)
                          (height gl-sprite)
                          1.0)))
             (values)))

    (defmethod render ((drawable gl-sprite) update-percent (camera simple-camera) (renderer gl-context))
      (declare (optimize (speed 3))
               (gl-sprite drawable)
               (simple-camera camera))
      (with-slots (color color-maps sprite-source flip-vector shader texture vao)
          drawable
        (declare ((simple-array single-float (2)) flip-vector)
                 ((or null color) color)
                 (shader shader)
                 (texture texture)
                 (integer vao)
                 ((or null sprite-source) sprite-source))
        (gl-use-shader renderer shader)

        (let ((map (if color-maps (elt color-maps 0) %no-op-color-map%)))
          (when (and color-maps (> (length color-maps) 1))
            (error "multiple color maps not supported yet."))
          (set-uniformf shader
                        "spriteColorMapFrom"
                        (r (color-map-from-color map))
                        (g (color-map-from-color map))
                        (b (color-map-from-color map))
                        (a (color-map-from-color map)))
          (set-uniformf shader
                        "spriteColorMapTo"
                        (r (color-map-to-color map))
                        (g (color-map-to-color map))
                        (b (color-map-to-color map))
                        (a (color-map-to-color map)))
          (set-uniformf shader
                        "spriteColorMapTolerance"
                        (color-map-tolerance map)))
        (if color
            (set-uniformf shader
                          "spriteColorMod"
                          (r color)
                          (g color)
                          (b color)
                          (a color))
            (set-uniformf shader
                          "spriteColorMod"
                          1.0 1.0 1.0 1.0))

        ;; set position, rotation, and size
        (update-world-model drawable update-percent)
        (set-uniform-matrix-4fv shader
                                "worldModel"
                                (slot-value drawable 'world-model)
                                nil)

        ;; set world projection
        (set-uniform-matrix-4fv shader
                                "worldProjection"
                                (projection-matrix camera)
                                nil)

        ;; set sprite source rectangle
        (let* ((source (or sprite-source %default-sprite-source%))
               (flip-x (elt flip-vector 0))
               (flip-y (elt flip-vector 1))
               (x (sprite-source-x source))
               (y (sprite-source-y source))
               (total-w (texture-src-width texture))
               (total-h (texture-src-height texture))
               (w (or (sprite-source-w source) total-w))
               (h (or (sprite-source-h source) total-h)))
          (declare ((single-float -1.0 1.0) flip-x flip-y)
                   ((integer 0 *) x y w h total-w total-h))
          (set-uniformf shader
                        "spriteSrc"
                        ;; x y width height
                        (if (< flip-x 0)
                            (/ (float (+ x w)) total-w)
                            (/ (float x) total-w))
                        (if (< flip-y 0)
                            (/ (float y) total-h)
                            ;; add src-h to y coord to render coords from upper-left corner instead of lower-left
                            (/ (float (+ h y)) total-h))
                        (float (/ (* w flip-x) total-w))
                        (float (/ (* h flip-y) total-h))))
        (gl-bind-texture renderer texture)
        (gl-use-vao renderer vao)
        (n-draw-arrays :triangle-fan 0 4)
        (values)))))

(defmethod load-resources ((drawable gl-sprite) (renderer gl-context))
  ;; TODO
  (with-slots (path-to-sprite shader-cache shader texture-cache texture buffer-cache vao vbo)
      drawable
    (setf shader
          (getcache-default %sprite-key%
                            shader-cache
                            (let ((shader (%create-sprite-shader)))
                              (load-resources shader renderer)
                              shader))
          texture
          (getcache-default path-to-sprite
                            texture-cache
                            (let ((texture (make-instance
                                            'texture
                                            :path-to-texture path-to-sprite)))
                              (load-resources texture renderer)
                              texture)))
    (destructuring-bind (cached-vao cached-vbo)
        (getcache-default %sprite-key%
                          buffer-cache
                          (%create-sprite-vao))
      (setf vao cached-vao
            vbo cached-vbo))))

(defmethod release-resources ((drawable gl-sprite))
  (with-slots (path-to-sprite buffer-cache vao vbo texture-cache texture shader-cache shader)
      drawable
    (stop-using-cached-resource drawable path-to-sprite texture-cache)
    (remcache %sprite-key% shader-cache)
    (remcache %sprite-key% buffer-cache)
    (setf shader nil
          texture nil
          vao 0
          vbo 0)))

(defun %create-sprite-shader ()
  (let* ((vertex-shader-source
          "#version 330 core
layout (location = 0) in vec3 screenPos;
layout (location = 1) in vec2 srcCoord;

out vec2 TexCoord;

uniform mat4 worldModel;
uniform mat4 worldProjection;

uniform vec4 spriteSrc;

void main()
{
  float spriteSrcX = spriteSrc.x;
  float spriteSrcY = spriteSrc.y;
  float spriteWidth = spriteSrc.z;
  float spriteHeight = spriteSrc.w;

  TexCoord = vec2(spriteSrcX + (srcCoord.x * spriteWidth), spriteSrcY + (srcCoord.y * spriteHeight));
  gl_Position = worldProjection * worldModel * vec4(screenPos, 1.0);
}")
         (fragment-shader-source
          "#version 330 core
out vec4 FragColor;

in vec2 TexCoord;

// texture sampler
uniform sampler2D ourTexture;
uniform vec4 spriteColorMod;
uniform vec4 spriteColorMapFrom;
uniform vec4 spriteColorMapTo;
uniform float spriteColorMapTolerance;

void main()
{
  vec4 colorMappedTexture = texture(ourTexture, TexCoord);
  if (spriteColorMapFrom != spriteColorMapTo) {
    if ((abs(spriteColorMapFrom.r - colorMappedTexture.r) <= spriteColorMapTolerance)
         && (abs(spriteColorMapFrom.g - colorMappedTexture.g) <= spriteColorMapTolerance)
         && (abs(spriteColorMapFrom.b - colorMappedTexture.b) <= spriteColorMapTolerance)
         && (abs(spriteColorMapFrom.a - colorMappedTexture.a) <= spriteColorMapTolerance)) {
      colorMappedTexture = spriteColorMapTo * colorMappedTexture;
    }
  }
  FragColor = spriteColorMod * colorMappedTexture;
}")
         (sprite-shader (make-instance 'shader
                                       :vertex-source vertex-shader-source
                                       :fragment-source fragment-shader-source)))
    sprite-shader))

(defun %create-sprite-vao ()
  (let ((vao 0)
        (vbo 0)
        ;; render sprites from upper-left coords
        ;; TODO: put coords transform in separate matrix
        (gl-vertices (alloc-gl-array  :float
                                      ;; positions             texture coords
                                      0.0   0.0  0.0          1.0  0.0 ; top right
                                      0.0  -1.0  0.0          1.0 -1.0 ; bottom right
                                     -1.0  -1.0  0.0          0.0 -1.0 ; bottom left
                                     -1.0   0.0  0.0          0.0  0.0 ; top left
                                     )))
    (unwind-protect
         (progn
           ;; TODO release resources on errors
           (setf vao (gl:gen-vertex-array)
                 vbo (gl:gen-buffer))

           (gl:bind-vertex-array vao)

           ;; put the vertices in the VBO
           (gl:bind-buffer :array-buffer vbo)
           (gl:buffer-data :array-buffer :static-draw gl-vertices)
           ;; position
           (gl:vertex-attrib-pointer 0 3 :float 0 (* 5 (cffi:foreign-type-size :float)) (* 0 (cffi:foreign-type-size :float)))
           (gl:enable-vertex-attrib-array 0)
           ;; texture coord
           (gl:vertex-attrib-pointer 1 3 :float 0 (* 5 (cffi:foreign-type-size :float)) (* 3 (cffi:foreign-type-size :float)))
           (gl:enable-vertex-attrib-array 1)

           ;; note: this is okay because vertex-attrib-pointer binds the vertex shader's input
           (gl:bind-buffer :array-buffer 0)
           (gl:bind-vertex-array 0)
           (list vao vbo))
      (gl:free-gl-array gl-vertices))))
