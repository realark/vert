;;;; Implement rendering using sdl2 texture and rectangle
(in-package :recurse.vert)

(deftype sdl-rotation-degrees ()
  "Counter-clockwise rotation of an sdl texture about its local center"
  ;; reverse sdl angle because our
  ;; rotations go clockwise
  '(double-float -360d0 0d0))

;; sdl-rectangle-drawable class should not be used anywhere outside of this file
(defclass sdl-rectangle-drawable (aabb)
  ((sdl-rectangle :accessor sdl-rectangle
                  :type sdl2-ffi:sdl-rect
                  :documentation "SDL Rendering rectangle"))
  (:documentation "A drawable which uses an sdl rectangle for rendering"))

(defmethod initialize-instance :after ((drawable sdl-rectangle-drawable) &rest args)
  (declare (ignore args))
  (with-accessors ((rect sdl-rectangle)) drawable
    (setf rect (sdl2:make-rect 0 0 1 1))))

(defmethod update :before ((drawable sdl-rectangle-drawable) delta-t-ms world-context)
  (declare (optimize (speed 3)))

  (values))

(defmethod render :before ((drawable sdl-rectangle-drawable) update-percent (camera simple-camera) renderer)
  ;; Update the sdl-rect relative to CAMERA's position
  (declare (optimize (speed 3))
           ((single-float 0.0 1.0) update-percent))
  (with-slots ((rect sdl-rectangle)) drawable
    (declare (sdl2-ffi:sdl-rect rect))
    (multiple-value-bind (screen-width screen-height)
        (world-to-screen-dimensions drawable camera)
      (setf (sdl2:rect-width rect) screen-width
            (sdl2:rect-height rect) screen-height))
    (multiple-value-bind (screen-x screen-y) (world-to-screen-cords drawable camera update-percent)
      (declare (screen-unit screen-x screen-y))
      (setf (sdl2:rect-x rect) screen-x
            (sdl2:rect-y rect) screen-y)))
  (values))

(defmethod recycle :after ((sdl-rectangle-drawable sdl-rectangle-drawable))
  (with-slots (last-positions) sdl-rectangle-drawable
    (setf (elt last-positions 0) %uninitialized-interpolated-value%)
    (setf (elt last-positions 1) %uninitialized-interpolated-value%)))

;;;; texture rendering

;; TODO: create a sprite class
;; TODO: isolate texture cache

;; sdl-texture-drawable is exposed to the rest of the engine and graphics module.
;; It is implemented with sdl2 but must not expose those implementation details.
(defclass sdl-texture-drawable (sdl-rectangle-drawable)
  ((sdl-source-rectangle :initform nil
                         :documentation "Source sdl rectangle.")
   (texture-cache-key
    :initform nil
    :reader texture-cache-key
    :documentation "The key to use for texture caching (tested with equalp).
A nil key will disable caching for this texture.")
   (texture :initform nil
            :reader texture
            :type sdl2-ffi:sdl-texture
            :documentation "An sdl texture")
   (wrap-width :initarg :wrap-width
               :initform nil
               :documentation "TODO")
   (wrap-height :initarg :wrap-height
                :initform nil
                :documentation "TODO")
   (flip-list :initarg :flip-list
              :initform '()
              :reader flip-list
              :type list
              :documentation "A list of flips to apply to the texture.
Must be :NONE, :HORIZONTAL, or :VERTICAL")
   (color-mod :initform nil
              :initarg :color-mod
              :reader color-mod
              :type color
              :documentation "Color modifier.")
   (%sdl-rotation-degrees :initform 0d0
                          :type sdl-rotation-degrees
                          :documentation "optimization to avoid boxing rotation for every call to render-copy-ex"))
  (:documentation "A drawable which loads pixels from an external resource into a texture for rendering."))

(defmethod initialize-instance :after ((drawable sdl-texture-drawable) &rest args)
  (declare (ignore args))
  (setf (color-mod drawable) (color-mod drawable))
  (with-slots (%sdl-rotation-degrees wrap-width wrap-height) drawable
    (when wrap-width (setf wrap-width (coerce wrap-width 'world-dimension)))
    (when wrap-height (setf wrap-height (coerce wrap-height 'world-dimension)))
    ;; FIXME: Setting sdl rotation is consing
    ;; (setf %sdl-rotation-degrees (coerce (- (rad->deg (rotation drawable))) 'sdl-rotation-degrees))
    )
  (with-slots (flip-list) drawable
    (let ((tmp-list flip-list))
      (setf flip-list '())
      (loop for dir in tmp-list do
           (flip drawable dir)))))

(defmethod (setf rotation) :after (value (sdl-texture-drawable sdl-texture-drawable))
  (with-slots (rotation %sdl-rotation-degrees) sdl-texture-drawable
    (declare (sdl-rotation-degrees %sdl-rotation-degrees)
             (rotation-degrees rotation))
    ;; FIXME: Setting sdl rotation is consing
    ;; (setf %sdl-rotation-degrees (coerce (- (rad->deg rotation)) 'sdl-rotation-degrees))
    ))

(defmethod (setf color-mod) (value (drawable sdl-texture-drawable))
  (unless value (setf value *white*))
  (setf (slot-value drawable 'color-mod) value))

(defmethod (setf texture-cache-key) (value (drawable sdl-texture-drawable))
  (unless (equalp value (texture-cache-key drawable))
    (with-slots (texture-cache-key texture) drawable
      (release-resources drawable)
      (setf texture-cache-key value)))
  (texture-cache-key drawable))

(defmethod render ((drawable sdl-texture-drawable) update-percent (camera camera) (renderer sdl2-ffi:sdl-renderer))
  (declare (optimize (speed 3)))
  (with-slots (texture %sdl-rotation-degrees color-mod flip-list (sdl-rect sdl-rectangle) sdl-source-rectangle wrap-width wrap-height width height)
      drawable
    (declare (sdl-rotation-degrees %sdl-rotation-degrees))
    (unless texture (load-resources drawable renderer))
    (sdl2:set-texture-color-mod texture
                                (r color-mod)
                                (g color-mod)
                                (b color-mod))
    (sdl2:set-texture-alpha-mod texture (a color-mod))

    (if (or wrap-width wrap-height)
        (let* ((screen-x (sdl2:rect-x sdl-rect))
               (screen-y (sdl2:rect-y sdl-rect))
               (screen-w (sdl2:rect-width sdl-rect))
               (screen-h (sdl2:rect-height sdl-rect))
               (wrap-screen-w (ceiling (* (scale camera) (or wrap-width width))))
               (wrap-screen-h (ceiling (* (scale camera) (or wrap-height height))))
               (render-x screen-x)
               (render-y screen-y))
          (declare (screen-unit screen-x screen-y screen-w screen-h wrap-screen-w wrap-screen-h render-x render-y))

          (setf (sdl2:rect-width sdl-rect) wrap-screen-w
                (sdl2:rect-height sdl-rect) wrap-screen-h)
          (loop :while (and (< render-x (+ screen-x screen-w)) (< render-y (+ screen-y screen-h))) :do
             (setf (sdl2:rect-x sdl-rect) render-x
                   (sdl2:rect-y sdl-rect) render-y)

             (sdl2-ffi.functions:sdl-render-copy-ex ; do rendering
              renderer
              texture
              sdl-source-rectangle
              sdl-rect
              %sdl-rotation-degrees
              nil
              (autowrap::mask-apply 'sdl2::sdl-renderer-flip flip-list))
             ;; update render target to next square
             (incf render-x wrap-screen-w)
             (unless (< render-x (+ screen-x screen-w))
               (setf render-x screen-x
                     render-y (+ render-y wrap-screen-h)))))
        ;; render-copy is coercing rotation to double-float and consing a lot
        ;; so we'll just invoke the ffi function directly
        (sdl2-ffi.functions:sdl-render-copy-ex
         renderer
         texture
         sdl-source-rectangle
         sdl-rect
         %sdl-rotation-degrees
         nil
         (autowrap::mask-apply 'sdl2::sdl-renderer-flip flip-list))))
  (values))

(defgeneric create-sdl-texture (sdl-texture-drawable rendering-context)
  (:documentation "Create an sdl2 texture out of SDL-TEXTURE-DRAWABLE out of RENDERING-CONTEXT"))

(defmethod release ((texture sdl2-ffi:sdl-texture))
  (sdl2:destroy-texture texture))

(defmethod load-resources ((drawable sdl-texture-drawable) (renderer sdl2-ffi:sdl-renderer))
  (with-slots (texture-cache-key texture) drawable
    (unless texture
      (if texture-cache-key
          (setf texture
                (use-cached-resource drawable
                                     texture-cache-key
                                     (create-sdl-texture drawable renderer)))
          ;; caching is disabled for drawable so just create the texture
          (setf texture (create-sdl-texture drawable renderer))))))

(defmethod release-resources ((drawable sdl-texture-drawable))
  (with-slots (texture-cache-key texture) drawable
    (when texture
      (if texture-cache-key
          (stop-using-cached-resource drawable texture-cache-key)
          ;; caching is disabled for drawable so just free the texture
          (release texture))
      (setf texture nil))))
