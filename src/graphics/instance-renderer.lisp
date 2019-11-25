(in-package :recurse.vert)

;;;; instance renderer api

;; FIXME this likely exceeds (/ (gl:get* :max-shader-storage-block-size) 16)
(defconstant +max-instance-size+ (expt 2 32)
  "Optimization. The maximum number of objects an instance-renderer can render.")

@export
(defclass instance-renderer (game-object)
  ()
  (:documentation "An object which renders drawables using opengl instance rendering."))

@export
(defclass instance-rendered-drawable (game-object)
  ((instance-renderer :initarg :instance-renderer
                      :initform (error ":instance-renderer required"))
   (sprite-source-flip-vector :initform (vector2 1.0 1.0)
                              :reader sprite-source-flip-vector)
   (sprite-source :initarg :sprite-source
                  :initform nil
                  :reader sprite-source)))

(defmethod color ((drawable instance-rendered-drawable))
  (color (slot-value drawable 'instance-renderer)))

(defmethod (setf color) (value (drawable instance-rendered-drawable))
  (setf (color (slot-value drawable 'instance-renderer)) value))

(defmethod load-resources ((drawable instance-rendered-drawable) context))
(defmethod release-resources ((drawable instance-rendered-drawable)))

(declaim (ftype (function (instance-renderer instance-rendered-drawable) (integer 0 #.+max-instance-size+)) instance-renderer-queue))
(defgeneric instance-renderer-queue (instance-renderer instanced-rendered-drawable)
  (:documentation "Queue INSTANCED-RENDERED-DRAWABLE to be rendered by INSTANCE-RENDERER.
Returns the instanced-id of drawables (i.e. its position in the instance queue with 0 being the first element)."))

@export
(defgeneric instance-renderer-reset (instance-renderer)
  (:documentation "Remove all queued objects from INSTANCE-RENDERER."))

(defmethod render-queue-add ((queue render-queue) (object instance-rendered-drawable))
  (declare (optimize (speed 3)))
  (with-slots ((instance-renderer instance-renderer)) object
    (let ((instance-id (instance-renderer-queue instance-renderer object)))
      (when (eql 0 instance-id)
        ;; if this is the first element in the instance renderer, add the renderer to the queue
        (render-queue-add queue instance-renderer)))))

;;;; instanced-static-sprite -- the thing that does the rendering

(defvar %instanced-sprite-key% 'instanced-gl-sprite)

@export
(defclass sprite-instance-renderer (instance-renderer)
  ((path-to-sprite :initarg :path-to-sprite
                   :initform (error ":path-to-sprite must be specified")
                   :reader path-to-sprite
                   :documentation "Path to the sprite file.")
   (shader :initform (getcache-default %instanced-sprite-key%
                                       *shader-cache*
                                       (make-instance 'shader
                                                      :vertex-source (get-builtin-shader-source 'instanced-sprite-shader.vert)
                                                      :fragment-source (get-builtin-shader-source 'instanced-sprite-shader.frag)))
           :documentation "Shader used to draw the sprite.")
   (texture :initform nil
            :documentation "opengl texture.")
   (vao :initform 0)
   (quad-vbo :initform 0)
   (transform-vbo :initform 0)
   (sprite-source-vbo :initform 0)
   (fill-pointer :initform 0)
   (objects-to-render :initform nil)
   (c-transform-array :initform nil)
   (c-sprite-source-array :initform nil)
   (buffers-dirty-p :initform nil
                    :documentation "When t, the c and gl buffers need to be refreshed to match the lisp buffer.")
   (cached-interpolation-value :initform 0.0)
   (color :initarg :color
          :initform nil
          :documentation "A color mod blended with the drawable. Nil has the same effect as *white*."
          :accessor color)
   (z :initarg :z
      :reader z
      :initform 0.0
      :documentation "The z-layer this object is rendering to. All objects rendered by this instance-renderer must have the same z-layer")
   (quad-padding :initarg :quad-padding
                 :initform 0.0
                 :documentation "Optional padding to add to the rendered quad.
Currently used to workaround bugs where a temporary gap can appear between adjacent objects due to float rounding")
   (static-objects-p :initarg :static-objects-p
                     :initform nil
                     :documentation "Optimization hint. Inform the renderer it will be rendering static objects."))
  (:documentation "An object which renders a large number of STATIC-SPRITEs using instanced rendering."))

(defmethod initialize-instance :after ((renderer sprite-instance-renderer) &key (initial-buffer-size 100))
  (%resize-sprite-instance-buffers renderer initial-buffer-size))

(defun %resize-sprite-instance-buffers (sprite-instance-renderer new-size)
  "Replace SPRITE-INSTANCE-RENDERER's internal buffers with buffers sized to NEW-SIZE. Existing elements will be copied over as long as new-size > old-size."
  (with-slots ((objects objects-to-render) fill-pointer c-transform-array c-sprite-source-array) sprite-instance-renderer
    (declare ((or null (simple-array instance-rendered-drawable)) objects))
    ;; skip resize if new-size == current-size
    (unless (and objects (= (array-total-size objects) new-size))
      (let ((new (make-array new-size
                             :element-type 'instance-rendered-drawable
                             :adjustable nil
                             :initial-element (make-instance 'instance-rendered-drawable
                                                             :instance-renderer nil
                                                             :object-id 'dummy-array-filler))))
        (when objects ; copy existing array elements to the new buffer
          (when (<= new-size fill-pointer)
            (log:warn "shrinking instance array size: ~A -> ~A" fill-pointer new-size))
          (loop :for i :from 0 :below (min fill-pointer new-size) :do
               (setf (elt new i)
                     (elt objects i))))
        (setf objects new)
        (when (or c-transform-array c-sprite-source-array)
          (%resize-c-buffers sprite-instance-renderer))
        (values)))))

(defun %resize-c-buffers (sprite-instance-renderer)
  "Update SPRITE-INSTANCE-RENDERER's c buffers to match the size of the lisp buffer."
  (labels ((resize (old-c-buffer array-type new-size)
             (if (and old-c-buffer
                      (= new-size (gl::gl-array-size old-c-buffer)))
                 old-c-buffer ; size unchanged, keep the same buffer
                 (progn
                   (when old-c-buffer
                     (gl:free-gl-array old-c-buffer))
                   (gl:alloc-gl-array array-type new-size)))))
    (with-slots ((objects objects-to-render)
                 buffers-dirty-p
                 c-transform-array
                 c-sprite-source-array)
        sprite-instance-renderer
      (setf c-transform-array
            (resize c-transform-array
                    :float
                    ;; each transform is a 4x4 matrix
                    (* (array-total-size objects) 16))
            c-sprite-source-array
            (resize c-sprite-source-array
                    :float
                    ;; each sprite-source is a vec4
                    (* (array-total-size objects) 4)))
      (setf buffers-dirty-p t)
      (values))))

(defmethod load-resources ((renderer sprite-instance-renderer) gl-context)
  (with-slots (path-to-sprite
               shader texture
               vao quad-vbo
               transform-vbo
               sprite-source-vbo
               (objects objects-to-render)
               c-transform-array
               c-sprite-source-array)
      renderer
    (unless (/= 0 vao)
      (labels ((create-static-buffers (quad-padding)
                 (log:info "creating gl buffers for instanced sprite rendering")
                 (let ((vao 0)
                       (quad-vbo 0)
                       (c-vertices (alloc-gl-array  :float
                                                    ;; positions             texture coords
                                                    (+ 0.0 quad-padding)   (+ 0.0 quad-padding)  0.0          1.0  0.0 ; top right
                                                    (+ 0.0 quad-padding)   (- -1 quad-padding)  0.0          1.0 -1.0 ; bottom right
                                                    (- -1 quad-padding)    (- -1 quad-padding)  0.0          0.0 -1.0 ; bottom left
                                                    (- -1 quad-padding)    (+ 0.0 quad-padding)  0.0          0.0  0.0 ; top left
                                                   )))
                   (unwind-protect
                        (progn
                          (setf vao (gl:gen-vertex-array)
                                quad-vbo (gl:gen-buffer))
                          (gl-use-vao *gl-context* vao)
                          (gl:bind-buffer :array-buffer quad-vbo)
                          (gl:buffer-data :array-buffer :static-draw c-vertices)
                          ;; position
                          (gl:vertex-attrib-pointer 0 3 :float 0 (* 5 (cffi:foreign-type-size :float)) (* 0 (cffi:foreign-type-size :float)))
                          (gl:enable-vertex-attrib-array 0)
                          ;; texture coord
                          (gl:vertex-attrib-pointer 1 2 :float 0 (* 5 (cffi:foreign-type-size :float)) (* 3 (cffi:foreign-type-size :float)))
                          (gl:enable-vertex-attrib-array 1)
                          (gl:bind-buffer :array-buffer 0)
                          (list vao quad-vbo))
                     (gl:free-gl-array c-vertices))))
               (create-instance-buffers (vao)
                 (let ((transform-vbo (gl:gen-buffer)))
                   ;; define transform buffer
                   (gl-use-vao *gl-context* vao)
                   (gl:bind-buffer :array-buffer transform-vbo)
                   (gl:enable-vertex-attrib-array 2)
                   ;; Note: max vertex attrib size is 4, so we have to break out the data into 4 separate attribs
                   ;; inside the shader we can reference the first index as a mat4 and everything works
                   ;; (gl:vertex-attrib-pointer 2 16 :float 0 (* 16 (cffi:foreign-type-size :float)) (* 0 (cffi:foreign-type-size :float)))
                   (gl:vertex-attrib-pointer 2 4 :float 0 (* 16 (cffi:foreign-type-size :float)) (* 0 4 (cffi:foreign-type-size :float)))
                   (gl:enable-vertex-attrib-array 3)
                   (gl:vertex-attrib-pointer 3 4 :float 0 (* 16 (cffi:foreign-type-size :float)) (* 1 4 (cffi:foreign-type-size :float)))
                   (gl:enable-vertex-attrib-array 4)
                   (gl:vertex-attrib-pointer 4 4 :float 0 (* 16 (cffi:foreign-type-size :float)) (* 2 4 (cffi:foreign-type-size :float)))
                   (gl:enable-vertex-attrib-array 5)
                   (gl:vertex-attrib-pointer 5 4 :float 0 (* 16 (cffi:foreign-type-size :float)) (* 3 4 (cffi:foreign-type-size :float)))

                   (gl:bind-buffer :array-buffer 0)
                   (%gl:vertex-attrib-divisor 2 1)
                   (%gl:vertex-attrib-divisor 3 1)
                   (%gl:vertex-attrib-divisor 4 1)
                   (%gl:vertex-attrib-divisor 5 1)

                   (let ((sprite-source-vbo (gl:gen-buffer)))
                     ;; define sprite-source buffer
                     (gl:bind-buffer :array-buffer sprite-source-vbo)
                     (gl:enable-vertex-attrib-array 6)
                     (gl:vertex-attrib-pointer 6 4 :float 0 (* 4 (cffi:foreign-type-size :float)) (* 0 4 (cffi:foreign-type-size :float)))
                     (gl:bind-buffer :array-buffer 0)
                     (%gl:vertex-attrib-divisor 6 1)

                     (values transform-vbo sprite-source-vbo)))))
        (getcache %instanced-sprite-key% *shader-cache*)
        (load-resources shader gl-context)
        (unless texture
          (setf texture
                (getcache-default path-to-sprite
                                  *texture-cache*
                                  (let ((texture (make-instance
                                                  'texture
                                                  :path-to-texture path-to-sprite)))
                                    (load-resources texture gl-context)
                                    texture))))
        (destructuring-bind (new-vao new-quad-vbo)
            (create-static-buffers (slot-value renderer 'quad-padding))
          (setf vao new-vao
                quad-vbo new-quad-vbo))
        (multiple-value-bind (t-vbo s-vbo) (create-instance-buffers vao)
          (setf transform-vbo t-vbo
                sprite-source-vbo s-vbo))
        (%resize-c-buffers renderer))))
  (values))

(defmethod release-resources ((renderer sprite-instance-renderer))
  (with-slots (path-to-sprite shader texture vao quad-vbo transform-vbo sprite-source-vbo c-transform-array c-sprite-source-array) renderer
    (unless (= 0 vao)
      (stop-using-cached-resource texture path-to-sprite *texture-cache*)
      (remcache %instanced-sprite-key% *shader-cache*)
      (setf vao 0
            quad-vbo 0)
      (gl:delete-buffers (list transform-vbo sprite-source-vbo))
      (setf transform-vbo 0
            sprite-source-vbo 0)
      (gl:free-gl-array c-transform-array)
      (gl:free-gl-array c-sprite-source-array)
      (setf c-transform-array nil
            c-sprite-source-array nil)))
  (values))

(defmethod instance-renderer-queue ((renderer sprite-instance-renderer) (drawable instance-rendered-drawable))
  "Tell RENDERER to render DRAWABLE in the next render batch."
  (declare (optimize (speed 3)))
  (with-slots ((objects objects-to-render) fill-pointer buffers-dirty-p c-transform-array c-sprite-source-array transform-vbo) renderer
    ;; (declare ((simple-array instance-rendered-drawable (0 #.+max-instance-size+)) objects))
    (declare ((simple-array instance-rendered-drawable (*)) objects))
    (let ((next-object-index fill-pointer))
      (declare ((integer 0 #.+max-instance-size+)  next-object-index))
      (cond ((= +max-instance-size+ next-object-index)
             (log:warn "instance render limit reached: ~A" +max-instance-size+)
             (return-from instance-renderer-queue))
            ((= (array-total-size objects) next-object-index)
             (log:info "double instance buffer ~A : ~A -> ~A"
                       renderer
                       next-object-index
                       (* 2 next-object-index))
             (%resize-sprite-instance-buffers renderer (* 2 next-object-index))))
      (unless (eq (elt objects next-object-index) drawable)
        (setf (elt objects next-object-index) drawable
              buffers-dirty-p t))
      (incf fill-pointer)
      next-object-index)))

(defmethod instance-renderer-reset ((renderer sprite-instance-renderer))
  (with-slots (fill-pointer) renderer
    (setf fill-pointer 0)))

(defmethod render ((renderer sprite-instance-renderer) update-percent camera gl-context)
  (declare (optimize (speed 3)))
  (with-slots (shader texture vao fill-pointer cached-interpolation-value color buffers-dirty-p static-objects-p) renderer
    (declare ((integer 0 #.+max-instance-size+) fill-pointer))
    (when (> fill-pointer 0)
      (gl-use-shader gl-context shader)
      (set-uniform-matrix-4fv shader
                              "worldProjection"
                              (interpolated-world-projection-matrix camera update-percent)
                              nil)
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
      (gl-bind-texture gl-context texture)
      (gl-use-vao gl-context vao)
      (unless (or static-objects-p
                  (float= update-percent cached-interpolation-value))
        (setf buffers-dirty-p t))
      (setf cached-interpolation-value update-percent)
      (when buffers-dirty-p
        (log:trace "~A refresh gl buffers" renderer)
        (with-slots ((objects objects-to-render) c-transform-array c-sprite-source-array transform-vbo sprite-source-vbo) renderer
          (declare ((simple-array instance-rendered-drawable) objects))
          (loop :for drawable-index :from 0 :below fill-pointer :do
               (locally (declare ((integer 0 #.+max-instance-size+) drawable-index))
                 (let ((drawable (elt objects drawable-index)))
                   (let* ((source (or (sprite-source drawable)
                                      *default-sprite-source*))
                          (flip-vector (the vector2 (sprite-source-flip-vector drawable)))
                          (flip-x (elt flip-vector 0))
                          (flip-y (elt flip-vector 1))
                          (x (sprite-source-x source))
                          (y (sprite-source-y source))
                          (total-w (texture-src-width texture))
                          (total-h (texture-src-height texture))
                          (w (or (sprite-source-w source) total-w))
                          (h (or (sprite-source-h source) total-h))
                          (c-offset (* 4 drawable-index)))
                     (declare ((single-float -1.0 1.0) flip-x flip-y)
                              ((integer 0 *) x y w h total-w total-h))
                     ;; x y width height
                     (setf (cffi:mem-aref (gl::gl-array-pointer c-sprite-source-array)
                                          :float c-offset)
                           (if (< flip-x 0)
                               (/ (float (+ x w)) total-w)
                               (/ (float x) total-w)))
                     (setf (cffi:mem-aref (gl::gl-array-pointer c-sprite-source-array)
                                          :float (+ c-offset 1))
                           (if (< flip-y 0)
                               (/ (float y) total-h)
                               ;; add src-h to y coord to render coords from upper-left corner instead of lower-left
                               (/ (float (+ h y)) total-h)))
                     (setf (cffi:mem-aref (gl::gl-array-pointer c-sprite-source-array)
                                          :float (+ c-offset 2))
                           (float (/ (* w flip-x) total-w)))
                     (setf (cffi:mem-aref (gl::gl-array-pointer c-sprite-source-array)
                                          :float (+ c-offset 3))
                           (float (/ (* h flip-y) total-h))))
                   (loop :with model-matrix = (interpolated-sprite-matrix drawable update-percent)
                      :for c-index :from (* drawable-index 16)
                      :for i :from 0 :below 16 :do
                        (locally (declare (fixnum c-index i)
                                          (matrix model-matrix))
                          ;; glaref setf is consing so we'll use the ffi directly
                          ;; (setf (gl:glaref c-transform-array c-index)
                          ;;       (elt model-matrix i))
                          (setf (cffi:mem-aref (gl::gl-array-pointer c-transform-array)
                                               :float
                                               c-index)
                                (elt model-matrix i)))))))
          ;;  send c-arrays to opengl
          (n-bind-buffer :array-buffer sprite-source-vbo)
          (n-buffer-data :array-buffer
                         (* 4
                            (the (integer 1 256) (cffi:foreign-type-size :float))
                            (the (integer 1 #.+max-instance-size+) fill-pointer))
                         (gl::gl-array-pointer c-sprite-source-array)
                         :dynamic-draw)
          (n-bind-buffer :array-buffer transform-vbo)
          (n-buffer-data :array-buffer
                         (* 16
                            (the (integer 1 256) (cffi:foreign-type-size :float))
                            (the (integer 1 #.+max-instance-size+) fill-pointer))
                         (gl::gl-array-pointer c-transform-array)
                         :dynamic-draw)
          (setf buffers-dirty-p nil)))
      (n-draw-arrays-instanced :triangle-fan 0 4 fill-pointer))
    (values)))
