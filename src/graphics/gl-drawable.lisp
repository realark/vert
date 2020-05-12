(in-package :recurse.vert)

(defclass gl-drawable ()
  ((enable-p :initform t
             :documentation "When nil, rendering will be a no-op.")
   (input-texture :initform nil)
   (output-texture :initform nil))
  (:documentation "A opengl drawable which renders into the currently bound FBO.
The RENDER method will implement the drawing.
If OUTPUT-TEXTURE is defined, the FBO's contents will be copied to the texutre once rendering completes."))

(defclass gl-pipeline ()
  ((effects :initform (make-array 0
                                  :adjustable t
                                  :fill-pointer 0
                                  :element-type 'gl-drawable)))
  (:documentation "A list of gl-drawables which render with the first feeding its output into a texture which is passed to the second's input, etc."))

(defun gl-pipeline-add-effect (gl-pipeline gl-drawable)
  ;; TODO
  )

(defmethod render ((pipeline gl-pipeline) update-percent camera rendering-context)
  #+nil
  (let ((current-fbo (get-current-fbo *gl-context*)))
    ;; TODO: add fast-path for a pipeline of size 1 with not output-texture (just render to the current FBO and call it good).
    (when has-more-than-one-effect-p
      'get-or-create-tmp-fbo
      'get-or-create-tmp-texutre
      'set-fbo-to-tmp-fbo
      'do-the-rendering
      'copy-new-fbo-to-tmp-texture
      (loop :for effect :in effects :do
           'pass-tmp-texutre-into-effect
           'render-effect-to-tmp-fbo
           'copy-tmp-fbo-to-tmp-texutre)
      'render-tmp-texture-into-fbo
      'finally--reset-fbo-to-original-fbo
      )))


;;;; TODO scratch vvv

#+nil
(progn
  ;; scratching out how to combine gl-pipeline with current static-sprite and

  (defclass static-sprite (gl-drawable transform)
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
