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
             (let ((orig-fbo (gl-context-fbo *gl-context*)))
               (unwind-protect
                    (with-tmp-framebuffer (input-fbo)
                      (with-tmp-framebuffer (output-fbo)
                        (gl-context-use-fbo *gl-context* input-fbo)
                        (gl:clear :color-buffer-bit)
                        (flet ((last-active-drawable-p (index)
                                 (loop :for i :from index :below (length drawables) :do
                                      (when (gl-drawable-enabled-p (elt drawables i))
                                        (return nil))
                                    :finally (return t))))
                          (loop :for i :from 0
                             :for drawable :across drawables :do
                               (when (gl-drawable-enabled-p drawable)
                                 (if (last-active-drawable-p (+ i 1))
                                     (gl-context-use-fbo *gl-context* orig-fbo)
                                     (gl-context-use-fbo *gl-context* output-fbo))
                                 (setf (gl-drawable-input-texture drawable)
                                       (framebuffer-texture-id input-fbo))
                                 (render drawable update-percent camera rendering-context)
                                 (rotatef input-fbo output-fbo))))))
                 (gl-context-use-fbo *gl-context* orig-fbo))))))))

;;;; TODO scratch vvv

#+nil
(progn
  (defclass tmp-pipe (obb gl-pipeline)
    ())

  (defparameter *pipe*
    (make-instance 'vert::tmp-pipe
                   :width 32
                   :height 32
                   :x (x *player*)
                   :y (- (y *player*) 32)))
  (add-to-scene *scene* *pipe*)
  )

#+nil
(progn
  ;; scratching out how to combine gl-pipeline with current static-sprite and

  (defclass gl-texture-quad ()
    ;; simply render a texture quad
    ())

  (defclass static-sprite (transform)
    ;; convert a sprite into a texture
    ;; at the front of the pipeline, put the texture-quad renderer
    ((effects :initform (make-instance 'gl-pipeline))))

  (defmethod render ((sprite static-sprite) update-percent camera rendering-context)
    ;; update transform and interpolate between frames

    ;; simple case -- no effects
    ;; - compute matrix and bind variables
    ;; - render

    ;; complex case -- effects
    ;; - get-or-create-tmp-FBO
    ;; - bind identity-matrix for transforms
    ;; - render local space into tmp-FBO
    ;; - call into pipeline
    ;; - (sprite + effects are new local-space rendered in tmp-FBO)
    ;; - get-or-create-tmp-texture
    ;; - copy tmp-FBO into tmp-texture
    ;; - re-bind original FBO. Done with tmp-FBO
    ;; - bind world and identity matrices
    ;; - remove coloring (already done in step 1)
    ;; - run tmp-texture through sprite shader
    ;; - sprite + effects are now in the original FBO. We're done!

    ))
