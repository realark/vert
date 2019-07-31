(in-package :recurse.vert)

;;;; Base Draw component class

@export-class
(defclass draw-component ()
  ()
  (:documentation "Component which draws a game-object"))

(defclass no-op-draw-component (draw-component)
  ())

(defmethod render ((no-op no-op-draw-component) update-percent camera context))

;;;; GL Sprite

(defvar %sprite-key% 'gl-sprite-component)

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

@export-class
(defclass gl-sprite (draw-component)
  ((static-sprite :initarg :static-sprite
                  :initform (error ":static-sprite required")
                  :documentation "game-object being drawn by this gl-sprite")
   (shader :initform (getcache-default %sprite-key%
                                       *shader-cache*
                                       (%create-sprite-shader))
           :documentation "Shader used to draw the sprite.")
   (texture :initform nil
            :accessor gl-sprite-texture
            :documentation "opengl texture. Loaded from location specified by static-sprite slot.")
   (vao :initform 0)
   (vbo :initform 0))
  (:documentation "A draw component which renders a portion of a sprite to the screen using opengl."))

@inline
(defun gl-sprite-set-base-sprite-data (gl-sprite static-sprite update-percent camera)
  "Send GL-SPRITE's source and position data to opengl."
  (declare (optimize (speed 3))
           (gl-sprite gl-sprite)
           (drawable static-sprite)
           ((single-float 0f0 10f0) update-percent)
           (simple-camera camera))
  (with-slots (color color-maps sprite-source sprite-source-flip-vector )
      static-sprite
    (with-slots (shader texture vao) gl-sprite
      (let ((gl-context *gl-context*))
        (declare ((simple-array single-float (2)) sprite-source-flip-vector)
                 ((or null color) color)
                 (shader shader)
                 (texture texture)
                 (integer vao)
                 ((or null sprite-source) sprite-source))
        (gl-use-shader gl-context shader)

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
        (set-uniform-matrix-4fv shader
                                "worldModel"
                                (interpolated-sprite-matrix static-sprite update-percent)
                                nil)

        ;; set world projection
        (set-uniform-matrix-4fv shader
                                "worldProjection"
                                (interpolated-world-projection-matrix camera update-percent)
                                nil)

        ;; set sprite source rectangle
        (let* ((source (or sprite-source %default-sprite-source%))
               (flip-x (elt sprite-source-flip-vector 0))
               (flip-y (elt sprite-source-flip-vector 1))
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
        (gl-bind-texture gl-context texture)
        (gl-use-vao gl-context vao)))))

(defmethod render ((gl-sprite gl-sprite) update-percent (camera simple-camera) context)
  (gl-sprite-set-base-sprite-data gl-sprite (slot-value gl-sprite 'static-sprite) update-percent camera)
  (n-draw-arrays :triangle-fan 0 4))

(defvar *sprite-buffer-cache*
  (getcache-default "sprite-buffer-cache"
                    *engine-caches*
                    (make-instance 'counting-cache
                                   :on-evict
                                   (lambda (sprite-key gl-buffers)
                                     (declare (ignore sprite-key))
                                     (destructuring-bind (cached-vao cached-vbo) gl-buffers
                                       (gl:delete-vertex-arrays (list cached-vao))
                                       (gl:delete-buffers (list cached-vbo))))))
  "Cache of gl-sprie's VAO and VBO")

(defmethod load-resources ((drawable gl-sprite) gl-context)
  (with-slots (static-sprite shader texture vao vbo)
      drawable
    (unless (/= 0 vao)
      (getcache %sprite-key% *shader-cache*)
      (load-resources shader gl-context)
      (setf texture
            (getcache-default (path-to-sprite static-sprite)
                              *texture-cache*
                              (let ((texture (make-instance
                                              'texture
                                              :path-to-texture (path-to-sprite static-sprite))))
                                (load-resources texture gl-context)
                                texture))))
    (destructuring-bind (cached-vao cached-vbo)
        (getcache-default %sprite-key%
                          *sprite-buffer-cache*
                          (%create-sprite-vao))
      (setf vao cached-vao
            vbo cached-vbo))))

(defmethod release-resources ((drawable gl-sprite))
  (with-slots (static-sprite shader texture vao vbo)
      drawable
    (unless (= 0 vao)
      (stop-using-cached-resource texture (path-to-sprite static-sprite) *texture-cache*)
      (remcache %sprite-key% *shader-cache*)
      (remcache %sprite-key% *sprite-buffer-cache*)
      (setf vao 0
            vbo 0))))

(defun %create-sprite-shader ()
  (make-instance 'shader
                 :vertex-source (get-builtin-shader-source 'sprite-shader.vert)
                 :fragment-source (get-builtin-shader-source 'sprite-shader.frag)))

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

           (gl-use-vao *gl-context* vao)

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
           (list vao vbo))
      (gl:free-gl-array gl-vertices))))

;;;; GL Polygon Drawer

(defvar %polygon-draw% nil)

(defclass polygon-draw (draw-component)
  ((shader :initform (getcache-default 'polygon-shader
                                       *shader-cache*
                                       (make-instance 'shader
                                                      :vertex-source
                                                      (get-builtin-shader-source 'polygon-shader.vert)
                                                      :fragment-source
                                                      (get-builtin-shader-source 'polygon-shader.frag))))
   (vao :initform 0)
   (vbo :initform 0))
  (:documentation "A draw component which renders a solid color polygon."))


(defmethod load-resources ((polygon-draw polygon-draw) context)
  (with-slots (shader vao vbo) polygon-draw
    (when (= 0 vao)
      (load-resources shader context)
      (let ((buffers (%create-polygon-vao)))
        (setf vao (first buffers))
        (setf vbo (second buffers))))))

(defmethod release-resources ((polygon-draw polygon-draw))
    (with-slots (shader vao vbo) polygon-draw
      (unless (= 0 vao)
        (release-resources shader)
        (gl:delete-vertex-arrays (list vao))
        (gl:delete-buffers (list vbo))
        (setf vao 0
              vbo 0))))

(on-engine-start ('create-polygon-draw-component)
  (setf %polygon-draw% (make-instance 'polygon-draw))
  (load-resources %polygon-draw% t))

(on-engine-stop ('cleanup-polygon-draw-component)
  (release-resources %polygon-draw%)
  (setf %polygon-draw% nil))

(defun %render-polygon (game-object color update-percent camera renderer)
  "Render GAME-OBJECT using the global polygon shader"
  (declare (optimize (speed 3)))
  ;; - bind the geometry shader,
  ;; - set color and position
  (with-slots (shader vao) %polygon-draw%
    (gl-use-shader renderer shader)

    (if color
        (set-uniformf shader
                      "color"
                      (r color)
                      (g color)
                      (b color)
                      (a color))
        (set-uniformf shader
                      "color"
                      1.0 1.0 1.0 1.0))

    ;; set position, rotation, and size
    (let* ((sprite-transform (sprite-transform game-object)))
      (declare (dynamic-extent sprite-transform))
      (set-uniform-matrix-4fv shader
                              "worldModel"
                              sprite-transform
                              nil))

    ;; set world projection
    (set-uniform-matrix-4fv shader
                            "worldProjection"
                            (interpolated-world-projection-matrix camera update-percent)
                            nil)

    (gl-use-vao renderer vao)
    ;; TODO draw for arbitrary polygons (only rectangles supported currently)
    (n-draw-arrays :triangle-fan 0 4)
    (values)))

(defun %create-polygon-vao ()
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

           (gl-use-vao *gl-context* vao)

           ;; put the vertices in the VBO
           (gl:bind-buffer :array-buffer vbo)
           (gl:buffer-data :array-buffer :static-draw gl-vertices)
           ;; position
           (gl:vertex-attrib-pointer 0 3 :float 0 (* 3 (cffi:foreign-type-size :float)) (* 0 (cffi:foreign-type-size :float)))
           (gl:enable-vertex-attrib-array 0)

           ;; note: this is okay because vertex-attrib-pointer binds the vertex shader's input
           (gl:bind-buffer :array-buffer 0)
           (list vao vbo))
      (gl:free-gl-array gl-vertices))))

;;;; Dev option to draw GL primitives for hitboxes

(defvar %hitbox-render-color% (make-color-rgba :r 255 :a 100))
(defvar %phantom-render-color% (make-color-rgba :b 255 :a 100))

(defmethod render :after ((game-object game-object) update-percent camera (renderer gl-context))
  (when (get-dev-config 'dev-mode-render-collision-hitboxes)
    (cond ((typep game-object 'phantom)
           (%render-polygon game-object %phantom-render-color% 1.0 camera renderer))
          ((not (eq game-object (hit-box game-object t)))
           (%render-polygon (hit-box game-object t) %hitbox-render-color% 1.0 camera renderer)))))
