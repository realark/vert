(in-package :recurse.vert)

(defclass gl-drawable ()
  ((enabled-p :initform t
              :accessor gl-drawable-enabled-p
              :documentation "When nil, rendering will be a no-op.")
   (input-texture :initform nil
                  :accessor gl-drawable-input-texture
                  :documentation "When this drawable is used in a GL-PIPELINE, the texture-id of the previous step will be bound in this slot before RENDER is called.
It is not required that steps actually do anything with the input-texture. They may discard it.
Slot will be nil if this drawable is the first stage in the pipeline.")

   ;; TODO
   ;; (render-offset :initform '(0 . 0)
   ;;                :documentation "offset for the drawable to start rendering at.")
   )
  (:documentation "A opengl drawable which renders into the currently bound FBO.
This component is designed to be used in a GL-PIPELINE, thought it doesn't have to be.
The RENDER method will implement the drawing.
If OUTPUT-TEXTURE is defined, the FBO's contents will be copied to the texutre once rendering completes."))

(defclass gl-pipeline ()
  ((drawables :initform (make-array 0
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

(defun gl-pipeline-add (gl-pipeline gl-drawable)
  "Append GL-DRAWABLE to the end of GL-PIPELINE's draw commands."
  (declare (gl-pipeline gl-pipeline)
           (gl-drawable gl-drawable))
  (with-slots (drawables) gl-pipeline
    (vector-push-extend gl-drawable drawables)))

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
  (let ((num-active-drawables (gl-pipeline-num-active pipeline)))
    (declare ((integer 0 1024) num-active-drawables))
    (with-slots (drawables) pipeline
      (cond ((= 0 num-active-drawables)) ; Nothing to draw.
            ((= 1 num-active-drawables)
             ;; fast path. Draw the single command into the current FBO
             (let ((drawable (%gl-pipeline-first-enabled-drawable pipeline)))
               (setf (gl-drawable-input-texture drawable) nil)
               (render drawable update-percent camera rendering-context)))
            (t
             ;; TODO: allow drawables to modify the transform matrix
             (let ((orig-fbo (gl-context-fbo *gl-context*)))
               (unwind-protect
                    (destructuring-bind (fbo-width fbo-height)
                        (or (getconfig 'game-resolution *config*)
                            (list 320 180))
                      ;; Note: Currently sizing the tmp FBOs based on the window size
                      ;; it would probably be more optimal to size on the game resolution (usually much smaller)
                      ;; and scale up in the final output. I'm not doing that right now because
                      ;; that's complicated (at least, doing it in a generic size agnostic way is complicated).
                      ;; Plus, I think scaling might look weird.
                      ;; I'll come back to this if performance becomes an issue.
                      (setf fbo-width
                            (* fbo-width
                               (ceiling (screen-width camera) fbo-width))
                            fbo-height
                            (* fbo-height
                               (ceiling (screen-height camera) fbo-height)))
                      (with-tmp-framebuffer (input-fbo :width fbo-width :height fbo-height)
                        (with-tmp-framebuffer (output-fbo :width fbo-width :height fbo-height)
                          (gl-context-use-fbo *gl-context* input-fbo)
                          (gl:viewport 0
                                       0
                                       fbo-width
                                       fbo-height)
                          (gl:clear :color-buffer-bit)
                          (flet ((last-active-drawable-p (index)
                                   (loop :for i :from index :below (length drawables) :do
                                        (when (gl-drawable-enabled-p (elt drawables i))
                                          (return nil))
                                      :finally (return t))))
                            (log:trace "~A pipeline rendering ~A drawables"
                                       pipeline
                                       num-active-drawables)
                            (loop :for i :from 0
                               :for drawable :across drawables :do
                                 (when (gl-drawable-enabled-p drawable)
                                   (if (last-active-drawable-p (+ i 1))
                                       (progn
                                         (gl-context-use-fbo *gl-context* orig-fbo)
                                         (set-gl-viewport-to-game-resolution (screen-width camera) (screen-height camera)))
                                       (progn
                                         (gl-context-use-fbo *gl-context* output-fbo)
                                         (gl:viewport 0
                                                      0
                                                      fbo-width
                                                      fbo-height)))
                                   (gl:clear :color-buffer-bit)
                                   (log:trace "-- rendering ~A from FBO ~A to FBO ~A"
                                              drawable
                                              (slot-value input-fbo 'id)
                                              (gl-context-fbo *gl-context*))
                                   (setf (gl-drawable-input-texture drawable)
                                         (framebuffer-texture-id input-fbo))
                                   (render drawable update-percent camera rendering-context)
                                   ;; feed the current drawable's output into the next drawable's input
                                   (rotatef input-fbo output-fbo)))))))
                 (gl-context-use-fbo *gl-context* orig-fbo)
                 (set-gl-viewport-to-game-resolution (screen-width camera) (screen-height camera)))))))))

;;;; color invert effect

(defclass gl-color-invert (gl-drawable)
  ;; FIXME vao can't be created outside of *gl-context*
  ;; FIXME need to call load-resources on the shader
  ((vao :initform (%create-inverter-vao))
   (shader :initform (make-instance 'shader
                                    :vertex-source (get-builtin-shader-source 'sprite-shader.vert)
                                    :fragment-source (get-builtin-shader-source 'inverter-shader.frag)))))

(defmethod render ((color-invert gl-color-invert) update-percent camera rendering-context)
  (with-slots (shader vao) color-invert
    (declare (shader shader)
             ;; (integer vao)
             )
    (gl-use-shader *gl-context* shader)
    (gl-use-vao *gl-context* (car vao))
    (gl-bind-texture *gl-context* nil)
    (n-bind-texture :texture-2d
                    (gl-drawable-input-texture color-invert))

    ;; set position, rotation, and size
    (set-uniform-matrix-4fv shader
                            "worldModel"
                            *identity-matrix*
                            nil)

    ;; set world projection
    (set-uniform-matrix-4fv shader
                            "worldProjection"
                            *identity-matrix*
                            nil)

    ;; set sprite source rectangle
    (set-uniformf shader
                  "spriteSrc"
                  ;; x y width height
                  0.0 0.0 1.0 1.0)
    (n-draw-arrays :triangle-fan 0 4)))

#+nil
(with-slots (vao shader enabled-p) (elt (slot-value *scene* 'drawables) 1)
        (gl:delete-vertex-arrays (list (car vao)))
        (gl:delete-buffers (cdr vao))
        (setf vao (%create-inverter-vao))

        (reload-all-shaders)
        (release-resources shader)
        (setf shader (make-instance 'shader
                                    :vertex-source (get-builtin-shader-source 'sprite-shader.vert)
                                    :fragment-source (get-builtin-shader-source 'inverter-shader.frag)))
        (load-resources shader)

        (setf enabled-p t)
        )

(defun %create-inverter-vao ()
  (let ((vao 0)
        (vbo 0)
        ;; render sprites from upper-left coords
        ;; TODO: put coords transform in separate matrix
        ;; TODO Use a local space which ranges the entire screen centered at 0,0
        (gl-vertices (alloc-gl-array  :float
                                      ;; positions             texture coords
                                      1.0  -1.0  0.0          1.0 -1.0 ; bottom right
                                      1.0   1.0  0.0          1.0  0.0 ; top right
                                      -1.0   1.0  0.0         0.0  0.0 ; top left
                                      -1.0  -1.0  0.0         0.0 -1.0 ; bottom left
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
