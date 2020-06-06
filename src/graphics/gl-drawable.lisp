(in-package :recurse.vert)

(defvar %fullscreen-ortho-matrix%
  (ortho-matrix 0.0  1.0
                1.0  0.0
                1.0 -1.0)
  "ortho matrix for rendering across the entire game window.
Verts coord system has 0,0 being the upper-left corner. Positive X goes right. Positive Y goes down.")

(defgeneric compute-transform (drawable-or-pipeline update-percent)
  (:documentation "Compute a transform matrix to pass to the pipeline's gl-drawables"))

@export-class
(defclass gl-drawable ()
  ((enabled-p :initform t
              :accessor gl-drawable-enabled-p
              :documentation "When nil, rendering will be a no-op.")
   (render-priority :initarg :render-priority
                    :initform 0
                    :accessor gl-drawable-render-priority
                    :documentation "A priority integer which determines the ordering of GL-DRAWABLEs in a gl-pipeline.
Lower numbers run first. Higher numbers run later. Defaults to zero.
Drawables with the same priority may run in any order.")
   (input-texture :initform nil
                  :accessor gl-drawable-input-texture
                  :documentation "When this drawable is used in a GL-PIPELINE, the texture-id of the previous step will be bound in this slot before RENDER is called.
It is not required that steps actually do anything with the input-texture. They may discard it.
Slot will be nil if this drawable is the first stage in the pipeline.")
   (transform :initarg :transform
              :initform nil
              :accessor gl-drawable-transform
              :documentation "Optional transform matrix to apply to the drawable. Nil will use the entire screen."))
  (:documentation "A opengl drawable which renders into the currently bound FBO.
This component is designed to be used in a GL-PIPELINE, thought it doesn't have to be.
The RENDER method will implement the drawing.
If OUTPUT-TEXTURE is defined, the FBO's contents will be copied to the texutre once rendering completes."))

@export
(defgeneric gl-drawable-modify-render-area (gl-drawable obb)
  (:documentation "Optional hook for a gl-drawable to modify the render-area when used in a pipeline.")
  (:method (gl-drawable obb)))

@export-class
(defclass gl-pipeline ()
  ((render-area :initform nil
                :accessor gl-pipeline-render-area
                :documentation "Optional OBB to render the pipeline into. Nil renders the entire screen.")
   (render-area-copy
    :initform (make-instance 'obb)
    :documentation "Copy of render-area obb. Used to allow modifications to how the render-area is drawn without affecting the game logic.")
   (transform-interpolator :initform (make-matrix-interpolator)
                           :documentation "When render-area is used, this slot hold the matrix interpolator for render-area's transform.")
   (drawables :initform (make-array 0
                                    :adjustable t
                                    :fill-pointer 0
                                    :element-type 'gl-drawable)))
  (:documentation "A list of gl-drawables which render with the first feeding its output into a texture which is passed to the second's input, etc."))

(defmethod load-resources ((pipeline gl-pipeline))
  (prog1 (call-next-method pipeline)
    (with-slots (drawables) pipeline
      (loop :for drawable :across drawables :do
           (load-resources drawable)))))

(defmethod release-resources ((pipeline gl-pipeline))
  (prog1 (call-next-method pipeline)
    (with-slots (drawables) pipeline
      (loop :for drawable :across drawables :do
           (release-resources drawable)))))

@inline
(defun %gl-pipeline-compute-render-area-copy (gl-pipeline)
  (with-slots (drawables render-area render-area-copy) gl-pipeline
    (setf (parent render-area-copy) (parent render-area)
          (x render-area-copy) (x render-area)
          (y render-area-copy) (y render-area)
          (z render-area-copy) (z render-area)
          (rotation render-area-copy) (rotation render-area)
          (width render-area-copy) (width render-area)
          (height render-area-copy) (height render-area))
    (loop :for drawable :across drawables :do
         (gl-drawable-modify-render-area drawable render-area-copy))
    render-area-copy))

(defmethod compute-transform ((pipeline gl-pipeline) update-percent)
  (declare (optimize (speed 3)))
  (with-slots (drawables render-area render-area-copy transform-interpolator) pipeline
    (if render-area
        (let ((transform (obb-render-transform
                          (%gl-pipeline-compute-render-area-copy pipeline))))
          (declare (dynamic-extent transform))
          (interpolator-compute transform-interpolator transform update-percent))
        *identity-matrix*)))

@inline
(defun %gl-pipeline-drop-matrix0 (pipeline)
  (declare (optimize (speed 3)))
  (with-slots (render-area transform-interpolator) pipeline
    (when render-area
      (let ((transform (obb-render-transform
                        (%gl-pipeline-compute-render-area-copy pipeline))))
        (declare (dynamic-extent transform))
        (interpolator-update transform-interpolator transform)))))

(defmethod pre-update ((pipeline gl-pipeline))
  (declare (optimize (speed 3)))
  (prog1 (call-next-method pipeline)
    (%gl-pipeline-drop-matrix0 pipeline)))

(defmethod recycle ((pipeline gl-pipeline))
  ;; drop the previous matrix
  (prog1 (call-next-method pipeline)
    (%gl-pipeline-drop-matrix0 pipeline)))

@export
(defun gl-pipeline-add (gl-pipeline gl-drawable)
  "Append GL-DRAWABLE to the end of GL-PIPELINE's draw commands."
  (declare (gl-pipeline gl-pipeline)
           (gl-drawable gl-drawable))
  (with-slots (drawables) gl-pipeline
    (vector-push-extend gl-drawable drawables)
    (setf drawables
          (sort drawables
                (lambda (d1 d2)
                  (< (gl-drawable-render-priority d1)
                     (gl-drawable-render-priority d2)))))))

(defun gl-pipeline-num-active (gl-pipeline)
  "Return the number of drawables in the pipeline which are actively rendering."
  (with-slots (drawables) gl-pipeline
    (loop :with count = 0
       :for drawable :across drawables :do
         (when (gl-drawable-enabled-p drawable)
           (incf count))
       :finally (return count))))

(defun %gl-pipeline-first-enabled-drawable (gl-pipeline)
  "Get the first enabled drawable in the pipeline."
  (with-slots (drawables) gl-pipeline
    (loop :for drawable :across drawables :do
         (when (gl-drawable-enabled-p drawable)
           (return drawable)))))

(defmethod update ((pipeline gl-pipeline))
  (prog1 (call-next-method pipeline)
    (with-slots (drawables) pipeline
      (loop :for drawable :across drawables :do
           (update drawable)))))

(defmethod render ((pipeline gl-pipeline) update-percent camera rendering-context)
  (declare (optimize (speed 3)))
  (let ((num-active-drawables (gl-pipeline-num-active pipeline)))
    (declare ((integer 0 1024) num-active-drawables))
    (with-slots (drawables) pipeline
      (declare ((vector gl-drawable) drawables))
      (cond ((= 0 num-active-drawables)) ; Nothing to draw.
            ((= 1 num-active-drawables)
             ;; fast path. Draw the single command into the current FBO
             (let ((drawable (%gl-pipeline-first-enabled-drawable pipeline)))
               (setf (gl-drawable-input-texture drawable)
                     nil
                     (gl-drawable-transform drawable)
                     (when (slot-value pipeline 'render-area)
                       (compute-transform pipeline update-percent)))
               (render drawable update-percent camera rendering-context)))
            (t
             (let ((orig-fbo (gl-context-fbo *gl-context*))
                   (orig-clear-color (clear-color *engine-manager*))
                   (orig-reset-p nil))
               (unwind-protect
                    (flet ((compute-fbo-dimensions (pipeline)
                             (declare (ignore pipeline))
                             ;; Note: Currently sizing the tmp FBOs based on the window size
                             ;; it would probably be more optimal to size on the game resolution (usually much smaller)
                             ;; and scale up in the final output. I'm not doing that right now because
                             ;; that's complicated (at least, doing it in a generic size agnostic way is complicated).
                             ;; Plus, I think scaling might look weird.
                             ;; I'll come back to this if performance becomes an issue.
                             (multiple-value-bind (x y w h)
                                 (compute-gl-viewport-for-game-resolution (screen-width camera)
                                                                          (screen-height camera))
                               (declare (ignore x y))
                               (values w h)))
                           (last-active-drawable-p (index)
                             (loop :for i :from index :below (length drawables) :do
                                  (when (gl-drawable-enabled-p (elt drawables i))
                                    (return nil))
                                :finally (return t))))
                      (declare (inline compute-fbo-dimensions))
                      (multiple-value-bind (fbo-width fbo-height) (compute-fbo-dimensions pipeline)
                        (with-tmp-framebuffer (input-fbo :width fbo-width :height fbo-height)
                          (with-tmp-framebuffer (output-fbo :width fbo-width :height fbo-height)
                            (block clear-input-fbo
                              (gl-context-use-fbo *gl-context* input-fbo)
                              (gl:viewport 0 0 fbo-width fbo-height)
                              (setf (clear-color *engine-manager*) *invisible*)
                              (gl:clear :color-buffer-bit))
                            (log:trace "~A pipeline rendering ~A drawables"
                                       pipeline
                                       num-active-drawables)
                            (loop :for i :from 0
                               :for drawable :across drawables :do
                                 (locally (declare ((integer 0 1024) i))
                                   (when (gl-drawable-enabled-p drawable)
                                     (setf (gl-drawable-transform drawable) nil)
                                     (if (last-active-drawable-p (+ i 1))
                                         (progn
                                           (gl-context-use-fbo *gl-context* orig-fbo)
                                           (setf (gl-drawable-transform drawable)
                                                 (when (slot-value pipeline 'render-area)
                                                   (compute-transform pipeline update-percent)))
                                           (set-gl-viewport-to-game-resolution (screen-width camera) (screen-height camera))
                                           (setf (clear-color *engine-manager*) orig-clear-color
                                                 orig-reset-p t))
                                         (progn
                                           (gl-context-use-fbo *gl-context* output-fbo)
                                           (gl:viewport 0 0 fbo-width fbo-height)
                                           (setf (clear-color *engine-manager*) *invisible*)
                                           (gl:clear :color-buffer-bit)))
                                     (log:trace "-- rendering ~A from FBO ~A to FBO ~A"
                                                drawable
                                                (slot-value input-fbo 'id)
                                                (gl-context-fbo *gl-context*))
                                     (setf (gl-drawable-input-texture drawable)
                                           (framebuffer-texture-id input-fbo))
                                     (render drawable update-percent camera rendering-context)
                                     (rotatef input-fbo output-fbo))))))))
                 (unless orig-reset-p
                   (gl-context-use-fbo *gl-context* orig-fbo)
                   (setf (clear-color *engine-manager*)
                         orig-clear-color)
                   (set-gl-viewport-to-game-resolution (screen-width camera) (screen-height camera))))))))))

;;;; quad texture base class
(defvar %quad-cache-key% 'gl-quad)

@export
(defconstant +quad-blend-mult+ 0)
@export
(defconstant +quad-blend-add+ 1)

@export-class
(defclass gl-quad (gl-drawable)
  ((buffer-ids :initform nil
               :documentation "(list vao-id vbo-id)")
   (shader :initform nil
           :documentation "quad shader. Subclasses may override and re-use the same vertex shader.")
   (quad-releaser :initform nil)
   (color :initarg :color
          :initform nil
          :accessor color
          :documentation "Optional color mod to apply to the quad. If a texture is also applied the two color values will be blended.")
   (color-blend-fn :initarg :color-blend-fn
                   :initform nil)
   (texture-src :initarg :texture-src
                :initform (vector 0.0 0.0 1.0 1.0)
                :documentation "x y width height. Normalized coords."))
  (:documentation "Render a quad (rectangle) to the screen.
A texture may be provided to render into the quad.
Most gl drawing utils will want to subclass and override the SHADER slot with custom fragment rendering."))

(defmethod initialize-instance :around ((quad gl-quad) &rest args)
  (declare (optimize (speed 3)))
  ;; NOTE: consing
  (let ((all-args (append (list quad) args)))
    (prog1 (apply #'call-next-method all-args)
      (resource-autoloader-add-object *resource-autoloader*
                                      (tg:make-weak-pointer quad)))))

(defmethod gl-quad-shader-cache-key ((quad gl-quad))
  (type-of quad))

@export
(defmethod gl-quad-make-shader ((quad gl-quad))
  (make-instance 'shader
                 :vertex-source (get-builtin-shader-source 'quad-shader.vert)
                 :fragment-source (get-builtin-shader-source 'quad-shader.frag)))

@export
(defgeneric gl-quad-on-render (quad)
  (:documentation "Hook for gl-quad subclasses. Runs before rendering draw commands.")
  (:method ((quad gl-quad))))

(defmethod load-resources ((quad gl-quad))
  (prog1 (call-next-method quad)
    (unless (slot-value quad 'quad-releaser)
      (with-slots (buffer-ids shader) quad
        (setf buffer-ids
              (getcache-default %quad-cache-key%
                                *sprite-buffer-cache*
                                (%create-quad-vao)))
        (setf shader
              (getcache-default (gl-quad-shader-cache-key quad)
                                *shader-cache*
                                (let ((s (gl-quad-make-shader quad)))
                                  (load-resources s)
                                  s))))
      (let ((shader-key (gl-quad-shader-cache-key quad)))
        (setf (slot-value quad 'quad-releaser)
              (make-resource-releaser (quad)
                (%release-quad-resources %quad-cache-key%
                                         shader-key)))))))

(defun %release-quad-resources (buffer-key shader-key)
  (remcache buffer-key *sprite-buffer-cache*)
  (remcache shader-key *sprite-buffer-cache*))

(defmethod release-resources ((quad gl-quad))
  (prog1 (call-next-method quad)
    (with-slots (quad-releaser) quad
      (when quad-releaser
        (%release-quad-resources %quad-cache-key%
                                 (gl-quad-shader-cache-key quad))
        (cancel-resource-releaser quad-releaser)
        (setf quad-releaser nil)))))

(defmethod render ((quad gl-quad) update-percent camera rendering-context)
  (declare (optimize (speed 3)))
  (with-slots (shader buffer-ids) quad
    (declare (shader shader)
             (list buffer-ids))
    (gl-use-shader *gl-context* shader)
    (gl-use-vao *gl-context* (car buffer-ids))
    (gl-bind-texture *gl-context* nil)

    ;; color
    (with-slots (color) quad
      (if color
          (set-uniformf shader
                        "colorMod"
                        (r color)
                        (g color)
                        (b color)
                        (a color))
          (set-uniformf shader
                        "colorMod"
                        1.0 1.0 1.0 1.0)))
    ;; input texture and blending function
    (with-slots (color-blend-fn) quad
      ;; bind texture if there is one
      (if (gl-drawable-input-texture quad)
          (n-bind-texture :texture-2d
                          (gl-drawable-input-texture quad))
          (n-bind-texture :texture-2d
                          0))
      ;; use configured blend slot, or set blending based on texture status
      (if color-blend-fn
          (set-uniformi shader
                        "colorBlendFn"
                        color-blend-fn)
          (if (gl-drawable-input-texture quad)
              (set-uniformi shader
                            "colorBlendFn"
                            +quad-blend-mult+)
              (set-uniformi shader
                            "colorBlendFn"
                            +quad-blend-add+))))

    ;; set position and size matrices
    (with-slots (transform) quad
      (set-uniform-matrix-4fv shader
                              "worldModel"
                              (if transform
                                  transform
                                  *identity-matrix*)
                              nil)
      ;; set world projection
      (set-uniform-matrix-4fv shader
                              "worldProjection"
                              (if transform
                                  (interpolated-world-projection-matrix camera update-percent)
                                  %fullscreen-ortho-matrix%)
                              nil))
    ;; set sprite source rectangle
    (with-slots (texture-src) quad
      (set-uniformf shader
                    "textureSrc"
                    ;; x y width height
                    (elt texture-src 0)
                    (elt texture-src 1)
                    (elt texture-src 2)
                    (elt texture-src 3)))
    ;; hook for subclasses
    (gl-quad-on-render quad)
    (n-draw-arrays :triangle-fan 0 4)))

(defun %create-quad-vao ()
  (assert *gl-context*)
  (let ((vao 0)
        (vbo 0)
        (gl-vertices (alloc-gl-array  :float
                                      ;; 0,0 == upper-left and 1,1 == lower-right
                                      0.0   0.0  0.0      0.0  0.0 ; top left
                                      0.0   1.0  0.0      0.0  1.0 ; bottom left
                                      1.0   1.0  0.0      1.0  1.0 ; bottom right
                                      1.0   0.0  0.0      1.0  0.0 ; top right
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

;;;; color invert effect

@export-class
(defclass gl-color-invert (gl-quad)
  ())

(defmethod gl-quad-make-shader ((inverter gl-color-invert))
  (make-instance 'shader
                 :vertex-source (get-builtin-shader-source 'quad-shader.vert)
                 :fragment-source (get-builtin-shader-source 'inverter-shader.frag)))

;;;; color grayscale effect

@export-class
(defclass gl-color-grayscale (gl-quad)
  ())

(defmethod gl-quad-make-shader ((grayscale gl-color-grayscale))
  (make-instance 'shader
                 :vertex-source (get-builtin-shader-source 'quad-shader.vert)
                 :fragment-source (get-builtin-shader-source 'grayscale-shader.frag)))

;;;; kernel effect

@export
(defconstant +gl-kernel-sharpen+
  (make-array 9
              :element-type 'single-float
              :initial-contents
              (list -1.0 -1.0 -1.0
                    -1.0  9.0 -1.0
                    -1.0 -1.0 -1.0)))

@export
(defconstant +gl-kernel-edge-detect+
    (make-array 9
                :element-type 'single-float
                :initial-contents
                (list 1.0   1.0  1.0
                      1.0  -8.0  1.0
                      1.0   1.0  1.0)))

@export
(defconstant +gl-kernel-blur+
    (make-array 9
                :element-type 'single-float
                :initial-contents
                (list (/ 1.0 16) (/ 2.0 16) (/ 1.0 16)
                      (/ 2.0 16) (/ 4.0 16) (/ 2.0 16)
                      (/ 1.0 16) (/ 2.0 16) (/ 1.0 16))))

@export
(defconstant +gl-kernel-no-op+
    (make-array 9
                :element-type 'single-float
                :initial-contents
                (list 0.0  0.0  0.0
                      0.0  1.0  0.0
                      0.0  0.0  0.0)))

@export-class
(defclass gl-kernel (gl-quad)
  ((kernel :initarg :kernel
           :initform +gl-kernel-no-op+))
  (:documentation "A gl effect which applies a kernel using the KERNEL slot.
Subclasses may set or override this slot."))

(defmethod gl-quad-make-shader ((kernel gl-kernel))
  (make-instance 'shader
                 :vertex-source (get-builtin-shader-source 'quad-shader.vert)
                 :fragment-source (get-builtin-shader-source 'kernel-shader.frag)))

(defmethod gl-quad-on-render ((kernel-effect gl-kernel))
  (with-slots (shader kernel) kernel-effect
    (set-uniform-matrix-1fv shader "kernel" kernel)))
