(in-package :recurse.vert)

;;;; use instanced rendering to draw a bunch of static sprites

(defvar %instanced-sprite-key% 'instanced-gl-sprite)

(defconstant +max-instance-size+ (expt 2 32)
  "The maximum number of objects an instance-renderer can render. Defined and enforced for optimization purposes.")

@export
(defclass instanced-static-sprite ()
  ((path-to-sprite :initarg :path-to-sprite
                   :initform (error ":path-to-sprite must be specified")
                   :accessor path-to-sprite
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
   (objects-to-render :initform (make-array 1000 ; TODO 100 is probably a better initial size
                                            :element-type 'drawable
                                            :adjustable t
                                            :fill-pointer 0
                                            :initial-element (make-instance 'static-sprite
                                                                            :path-to-sprite "")))
   (c-transform-array :initform nil))
  (:documentation "An object which renders a large number of STATIC-SPRITEs using instanced rendering."))

(defmethod load-resources ((instanced-static-sprite instanced-static-sprite) gl-context)
  (with-slots (path-to-sprite
               shader texture
               vao quad-vbo
               transform-vbo
               sprite-source-vbo
               (objects objects-to-render)
               c-transform-array)
      instanced-static-sprite
    (unless (/= 0 vao)
      (labels ((create-shared-buffers ()
                 (log:info "creating gl buffers for instanced sprite rendering")
                 (let ((vao 0)
                       (quad-vbo 0)
                       (c-vertices (alloc-gl-array  :float
                                                    ;; positions             texture coords
                                                    0.0   0.0  0.0          1.0  0.0 ; top right
                                                    0.0  -1.0  0.0          1.0 -1.0 ; bottom right
                                                   -1.0  -1.0  0.0          0.0 -1.0 ; bottom left
                                                   -1.0   0.0  0.0          0.0  0.0 ; top left
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
                   transform-vbo)))
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
        (destructuring-bind (cached-vao cached-quad-vbo)
            (getcache-default %instanced-sprite-key%
                              *sprite-buffer-cache*
                              (create-shared-buffers))
          (setf vao cached-vao
                quad-vbo cached-quad-vbo))
        (setf transform-vbo (create-instance-buffers vao))
        (setf c-transform-array (gl:alloc-gl-array :float
                                                   (* (length objects)
                                                      ;; each transform is a 4x4 matrix
                                                      16))))))
  (values))

(defmethod release-resources ((instanced-static-sprite instanced-static-sprite))
  (with-slots (path-to-sprite shader texture vao quad-vbo transform-vbo sprite-source-vbo c-transform-array) instanced-static-sprite
    (unless (= 0 vao)
      (stop-using-cached-resource texture path-to-sprite *texture-cache*)
      (remcache %instanced-sprite-key% *shader-cache*)
      (remcache %instanced-sprite-key% *sprite-buffer-cache*)
      (setf vao 0
            quad-vbo 0)
      (gl:delete-buffers (list transform-vbo))
      (setf transform-vbo 0)
      (gl:free-gl-array c-transform-array)))
  (values))

(defun render-prepare (instanced-static-sprite drawable update-percent)
  "Tell INSTANCED-STATIC-SPRITE to render DRAWABLE in the next render batch."
  (declare (optimize (speed 3)))
  (with-slots ((objects objects-to-render) c-transform-array) instanced-static-sprite
    (declare ((vector drawable) objects))
    (let ((next-object-index (fill-pointer objects)))
      (declare ((integer 0 #.+max-instance-size+)  next-object-index))
      (cond ((= +max-instance-size+ next-object-index)
             (error "instance render limit reached: ~A" +max-instance-size+))
            ((= (array-total-size objects) next-object-index)
             (error "TODO: resize arrays")))
      (vector-push drawable objects)
      (loop :with model-matrix = (interpolated-sprite-matrix drawable update-percent)
         :for c-index :from (* next-object-index 16)
         :for i :from 0 :below 16 :do
           (locally (declare (fixnum c-index i)
                             (matrix model-matrix))
             ;; glaref setf is consing so we'll use the ffi directly
             ;; (setf (gl:glaref c-transform-array c-index)
             ;;       (elt model-matrix i))
             (setf (cffi:mem-aref (gl::gl-array-pointer c-transform-array)
                                  :float
                                  c-index)
                   (elt model-matrix i))))
      next-object-index)))

(defmethod render ((instanced-static-sprite instanced-static-sprite) update-percent camera gl-context)
  (declare (optimize (speed 3)))
  (with-slots (shader texture vao) instanced-static-sprite
    (gl-use-shader gl-context shader)
    (set-uniform-matrix-4fv shader
                            "worldProjection"
                            (interpolated-world-projection-matrix camera update-percent)
                            nil)
    (gl-bind-texture gl-context texture)
    (gl-use-vao gl-context vao)
    (with-slots (c-transform-array (objects objects-to-render) transform-vbo) instanced-static-sprite
      (declare (vector objects))
      ;;  send c-array to opengl
      (n-bind-buffer :array-buffer transform-vbo)
      (n-buffer-data :array-buffer
                     (* 16
                        (the (integer 1 256) (cffi:foreign-type-size :float))
                        (the (integer 1 #.+max-instance-size+) (length objects)))
                     (gl::gl-array-pointer c-transform-array)
                     :dynamic-draw)

      (n-draw-arrays-instanced :triangle-fan 0 4 (length objects))
      (setf (fill-pointer objects) 0))
    (values)))

(defclass test-renderer (instanced-static-sprite obb)
  ())

(defclass test-obj (recurse.syn::rectangle)
  ((instance-renderer :initarg :instance-renderer
                      :initform (error ":instance-renderer required"))))

(defmethod render ((obj test-obj) update-percent camera gl-context)
  (with-slots ((ir instance-renderer)) obj
    (render-prepare ir obj update-percent)
    (render ir update-percent camera gl-context)
    ;; (setf (fill-pointer (slot-value ir 'objects-to-render)) 0)
    #+nil
    (break "TODO: ~A" ir))

  )

(defmethod update :before ((object test-renderer) timestep scene)
  ;; (break "hello")
  )

(defmethod pre-update ((object test-renderer))
  ;; (break "hello")
  )

#+nil
(on-game-thread
  (defparameter *ir* (make-instance 'recurse.vert::test-renderer
                                    :path-to-sprite (resource-path "rectangle.png")))
  (load-resources *ir* recurse.vert::*gl-context*)

  (defparameter *obj* (make-instance 'recurse.vert::test-obj
                                     :width 10
                                     :height 20
                                     :x 30
                                     :y 40
                                     :z 1
                                     :instance-renderer *ir*))
  (add-to-scene *scene* *obj*))

#+nil
(on-game-thread
  (loop :for row :from 0 :below 100 :do
       (add-to-scene *scene*
                     (make-instance 'recurse.vert::test-obj
                                    :width 10
                                    :height 20
                                    :x (* 21 row)
                                    :y 40
                                    :z 1
                                    :instance-renderer *ir*))))
