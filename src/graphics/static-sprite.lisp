(in-package :recurse.vert)

;;;; gl-sprite draw-component

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

(defvar *default-sprite-source*
  (make-sprite-source 0 0 nil nil)
  "A sprite-source which shows the entire sprite")

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
   (shader :initform nil
           :documentation "Shader used to draw the sprite.")
   (texture :initform nil
            :accessor gl-sprite-texture
            :documentation "opengl texture. Loaded from location specified by static-sprite slot.")
   (vao :initform 0)
   (vbo :initform 0)
   (releaser :initform nil))
  (:documentation "A draw component which renders a portion of a sprite to the screen using opengl."))

(defmethod initialize-instance :around ((gl-sprite gl-sprite) &rest args)
  ;; TODO use push instead of append
  (let ((all-args (append (list gl-sprite) args)))
    (prog1 (apply #'call-next-method all-args)
      (with-slots (static-sprite) gl-sprite
        (resource-autoloader-add-object
         *resource-autoloader*
         (tg:make-weak-pointer gl-sprite)
         ;; static-sprite may still be initializing.
         ;; if so, skip the load. static-sprite will load the gl-sprite
         ;; when it finishes initializing
         :skip-object-load (null (path-to-sprite static-sprite)))))))

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
        (let* ((source (or sprite-source *default-sprite-source*))
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

(defmethod load-resources ((drawable gl-sprite))
  (unless (slot-value drawable 'releaser)
    (with-slots (static-sprite shader texture vao vbo)
        drawable
      (setf shader
            (getcache-default %sprite-key%
                              *shader-cache*
                              (%create-sprite-shader)))
      (load-resources shader)
      (setf texture
            (getcache-default (path-to-sprite static-sprite)
                              *texture-cache*
                              (make-instance
                               'texture
                               :path-to-texture (path-to-sprite static-sprite))))
      (load-resources texture)
      (destructuring-bind (cached-vao cached-vbo)
          (getcache-default %sprite-key%
                            *sprite-buffer-cache*
                            (%create-sprite-vao))
        (setf vao cached-vao
              vbo cached-vbo)))
    ;; note: using let instead of with-slots to avoid reference circularity
    (let ((path-to-sprite (path-to-sprite (slot-value drawable 'static-sprite)))
          (texture (slot-value drawable 'texture))
          (vao (slot-value drawable 'vao))
          (vbo (slot-value drawable 'vbo)))
      (setf (slot-value drawable 'releaser)
            (make-resource-releaser (drawable)
              (%release-gl-sprite-resources
               path-to-sprite
               texture
               vao
               vbo))))))

(defmethod release-resources ((drawable gl-sprite))
    (with-slots (releaser static-sprite shader texture vao vbo)
        drawable
      (when releaser
        (%release-gl-sprite-resources (path-to-sprite static-sprite) texture vao vbo)
        (cancel-resource-releaser releaser)
        (setf vao 0
              vbo 0
              shader nil
              texture nil
              releaser nil))))

(defun %release-gl-sprite-resources (path-to-sprite texture vao vbo)
  (declare (ignorable vao vbo))
  (when *gl-context*
    (stop-using-cached-resource texture path-to-sprite *texture-cache*)
    (remcache %sprite-key% *shader-cache*)
    (remcache %sprite-key% *sprite-buffer-cache*)))

(defun %create-sprite-shader ()
  (make-instance 'shader
                 :vertex-source (get-builtin-shader-source 'sprite-shader.vert)
                 :fragment-source (get-builtin-shader-source 'sprite-shader.frag)))

(defun %create-sprite-vao ()
  (let ((vao 0)
        (vbo 0)
        ;; render sprites from upper-left coords
        ;; TODO: put coords transform in separate matrix
        ;; TODO Use a local space which ranges the entire screen centered at 0,0
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
           (gl:vertex-attrib-pointer 1 2 :float 0 (* 5 (cffi:foreign-type-size :float)) (* 3 (cffi:foreign-type-size :float)))
           (gl:enable-vertex-attrib-array 1)

           ;; note: this is okay because vertex-attrib-pointer binds the vertex shader's input
           (gl:bind-buffer :array-buffer 0)
           (list vao vbo))
      (gl:free-gl-array gl-vertices))))

;;;; Static Sprite (Game object drawn with a gl-sprite draw-component)

(progn
  (defclass static-sprite (drawable obb)
    ((draw-component :initform nil)
     ;; static-sprite needs a sprite-draw-component to get texture width.
     ;; it's possible for subclasses to replace the draw-component so we'll store the sprite
     ;; component in a separate slot to be safe.
     (sprite-draw-component :initarg :sprite-draw-component
                            :initform nil)
     (path-to-sprite :initarg :path-to-sprite
                     :initform (error ":path-to-sprite must be specified")
                     :accessor path-to-sprite
                     :documentation "Path to the sprite file.")
     (color-maps :initform nil
                 :documentation "Before color mod is applied, allow mapping src colors to dest colors")
     (sprite-source :initarg :sprite-source
                    :initform nil
                    :documentation "Rectangle subset of the sprite to render.
Nil to render the entire sprite."
                    :accessor sprite-source)
     (sprite-source-flip-vector :initform (vector2 1.0 1.0)
                                :reader sprite-source-flip-vector)
     (wrap-width :initform nil
                 :initarg :wrap-width)
     (wrap-height :initform nil
                  :initarg :wrap-height)
     ;; TODO: consting vv
     (flip-list :initform (list)
                :accessor flip-list)
     (releaser :initform nil))
    (:documentation "A game-object which loads pixels from an image resource for rendering."))

  (export '(sprite-source sprite-source-x sprite-source-y sprite-source-width sprite-source-height)))

(defmethod initialize-instance :after ((sprite static-sprite) &rest args)
  (declare (ignore args))
  (with-slots (sprite-draw-component
               path-to-sprite
               sprite-source
               wrap-width
               wrap-height)
      sprite
    (unless (eq :skip sprite-draw-component)
      (setf sprite-draw-component (make-instance 'gl-sprite :static-sprite sprite)
            (draw-component sprite) sprite-draw-component))
    (when (and (or wrap-width wrap-height)
               sprite-source)
      (error "using wrap-width/height with sprite-source not supported. ~A" sprite))))

;; register with resource-autoloader
(defmethod initialize-instance :around ((static-sprite static-sprite) &rest args)
  (let ((all-args (append (list static-sprite) args)))
    (prog1 (apply #'call-next-method all-args)
      (static-sprite-register-resource-autoload static-sprite))))

(defmethod static-sprite-register-resource-autoload ((static-sprite static-sprite))
  (resource-autoloader-add-object *resource-autoloader*
                                  (tg:make-weak-pointer static-sprite)))

(defmethod (setf path-to-sprite) :around (new-sprite-path (static-sprite static-sprite))
  (let ((old-path (path-to-sprite static-sprite)))
    (prog1 (call-next-method new-sprite-path static-sprite)
      (unless (or (equal old-path (path-to-sprite static-sprite))
                  (null *engine-manager*))
        (release-resources static-sprite)
        (load-resources static-sprite)))))

(defmethod load-resources ((sprite static-sprite))
  (unless (slot-value sprite 'releaser)
    (with-slots (sprite-draw-component path-to-sprite wrap-width wrap-height)
        sprite
      (load-resources sprite-draw-component)
      (load-resources (draw-component sprite))

      (with-accessors ((sprite-source sprite-source)
                       (width width) (height height))
          sprite
        (with-accessors ((texture gl-sprite-texture))
            sprite-draw-component
          (when (or wrap-width wrap-height)
            (setf sprite-source
                  (make-sprite-source 0
                                      0
                                      (round
                                       (* (texture-src-width texture)
                                          (/ width (or wrap-width width))))
                                      (round
                                       (* (texture-src-height texture)
                                          (/ height (or wrap-height height))))))))))
    (setf (slot-value sprite 'releaser) t)))

(defmethod release-resources ((sprite static-sprite))
  (with-slots (releaser sprite-draw-component) sprite
    (when releaser
      (release-resources sprite-draw-component)
      (release-resources (draw-component sprite))
      (setf releaser nil))))

@export
(defun add-color-map (static-sprite color-map)
  "Apply COLOR-MAP to STATIC-SPRITE. See doc for color-map struct for details."
  (declare (static-sprite static-sprite)
           (color-map color-map))
  (with-slots (color-maps) static-sprite
    (if color-maps
        (error "multiple color maps not yet supported")
        (setf color-maps (make-array 1
                                     :element-type 'color-map
                                     :initial-contents (list color-map)
                                     :adjustable t)))))

@export
(defun get-color-maps (static-sprite)
  (slot-value static-sprite 'color-maps))

@export
(defmethod flip (sprite direction)
  "Toggle STATIC-SPRITE in the given DIRECTION.
A DIRECTION of :NONE will clear all flips"
  (declare (static-sprite sprite) (keyword direction))
  (with-slots (flip-list sprite-source-flip-vector) sprite
    (ecase direction
      (:none (setf flip-list (list)))
      ((:horizontal :vertical)
       (if (find direction flip-list)
           (setf flip-list (delete direction flip-list))
           (push direction flip-list))))
    (if (find :horizontal flip-list)
        (setf (elt sprite-source-flip-vector 0) -1.0)
        (setf (elt sprite-source-flip-vector 0) 1.0))
    (if (find :vertical flip-list)
        (setf (elt sprite-source-flip-vector 1) -1.0)
        (setf (elt sprite-source-flip-vector 1) 1.0))))
