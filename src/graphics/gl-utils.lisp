(in-package :recurse.vert)

(defun alloc-gl-array (type &rest data)
  (declare (dynamic-extent data))
  (loop :with gl-array = (gl:alloc-gl-array type (length data))
     :for val :in data
     :for i :from 0 :do
       (setf (gl:glaref gl-array i) val)
     :finally (return gl-array)))

(defun alloc-gl-array-from-vector (type lisp-vector)
  (loop :with gl-array = (gl:alloc-gl-array type (length lisp-vector))
     :for val :across lisp-vector
     :for i :from 0 :do
       (setf (gl:glaref gl-array i) val)
     :finally (return gl-array)))

;;;; Context Manager

(defstruct gl-context
  "Stores and sets options for the underlying opengl context with various optimizations. "
  (wrapper (error ":wrapper required") :type sdl2-ffi::sdl-glcontext)
  (shader nil :type (or null shader))
  (texture nil :type (or null texture))
  (fbo 0 :type integer)
  (vao 0 :type integer))

(defun gl-use-shader (gl-context shader)
  (declare (optimize (speed 3))
           (gl-context gl-context)
           (shader shader))
  (unless (and (eq (gl-context-shader gl-context) shader)
               (equal (shader-program-id (gl-context-shader gl-context))
                      (shader-program-id shader)))
    (setf (gl-context-shader gl-context) shader)
    (n-use-program (shader-program-id shader)))
  (values))

(defun gl-use-vao (gl-context vao)
  (declare (optimize (speed 3))
           (gl-context gl-context)
           (fixnum vao))
  (unless (= (gl-context-vao gl-context) vao)
    (setf (gl-context-vao gl-context) vao)
    (n-bind-vertex-array vao))
  (values))

(defun gl-bind-texture (gl-context texture)
  (declare (optimize (speed 3))
           (gl-context gl-context)
           ((or null texture) texture))
  (when (or (not (eq (gl-context-texture gl-context) texture))
            (not (equal (if (gl-context-texture gl-context)
                            (texture-id (gl-context-texture gl-context))
                            0)
                        (if texture
                            (texture-id texture)
                            0))))
    (setf (gl-context-texture gl-context) texture)
    (n-bind-texture
     :texture-2d
     (if texture (texture-id texture) 0)))
  (values))

(defun gl-context-use-fbo (gl-context fbo)
  "Binds the read-write framebuffer for the gl context"
  (declare (optimize (speed 3)))
  (let ((fbo-id (if (typep fbo 'framebuffer)
                    (slot-value fbo 'id)
                    fbo)))
    (declare (fixnum fbo-id))
    (unless (= (gl-context-fbo gl-context) fbo-id)
      (n-bind-framebuffer :framebuffer fbo-id)
      (setf (gl-context-fbo gl-context) fbo-id))))

(defun gl-context-clear-all (gl-context)
  (setf (gl-context-shader gl-context) nil
        (gl-context-texture gl-context) nil
        (gl-context-vao gl-context) 0
        (gl-context-fbo gl-context) 0))

;;;; Shader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct shader-source
    (source-file (error ":source-file required"))
    (file-contents nil :type (or null string)))

  (defun shader-source-load (shader-source)
    (declare (shader-source shader-source))
    (if (and (shader-source-source-file shader-source)
             (probe-file (shader-source-source-file shader-source)))
        (setf (shader-source-file-contents shader-source)
              (uiop:read-file-string (shader-source-source-file shader-source)))
        (warn "Unable to find shader source file: ~A. Not changing in-memory source."
              (shader-source-source-file shader-source))))

  (defvar %builtin-shaders% (make-hash-table))

  @export
  (defun add-builtin-shader-source (name path-to-shader)
    (let* ((path (probe-file path-to-shader))
           (shader-source
            (make-shader-source :source-file path)))
      (assert path)
      (when (gethash name %builtin-shaders%)
        (error "shader already defined"))

      (shader-source-load shader-source)
      (setf (gethash name %builtin-shaders%)
            shader-source)))

  @export
  (defun get-builtin-shader-source (name)
    (gethash name %builtin-shaders%))

  (when (= 0 (hash-table-count %builtin-shaders%))
    (add-builtin-shader-source 'polygon-shader.vert
                               (merge-pathnames (pathname "src/graphics/polygon-shader.vert")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'polygon-shader.frag
                               (merge-pathnames (pathname "src/graphics/polygon-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'sprite-shader.vert
                               (merge-pathnames (pathname "src/graphics/sprite-shader.vert")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'sprite-shader.frag
                               (merge-pathnames (pathname "src/graphics/sprite-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'font-shader.vert
                               (merge-pathnames (pathname "src/graphics/font-shader.vert")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'font-shader.frag
                               (merge-pathnames (pathname "src/graphics/font-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'instanced-sprite-shader.vert
                               (merge-pathnames (pathname "src/graphics/instanced-sprite-shader.vert")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'instanced-sprite-shader.frag
                               (merge-pathnames (pathname "src/graphics/instanced-sprite-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'quad-shader.vert
                               (merge-pathnames (pathname "src/graphics/quad-shader.vert")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'quad-shader.frag
                               (merge-pathnames (pathname "src/graphics/quad-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'inverter-shader.frag
                               (merge-pathnames (pathname "src/graphics/inverter-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'grayscale-shader.frag
                               (merge-pathnames (pathname "src/graphics/grayscale-shader.frag")
                                                (asdf:system-source-directory :vert)))
    (add-builtin-shader-source 'kernel-shader.frag
                               (merge-pathnames (pathname "src/graphics/kernel-shader.frag")
                                                (asdf:system-source-directory :vert)))))

(defclass shader ()
  ((vertex-source
    :initarg :vertex-source
    :initform (error ":vertex-source required"))
   (geometry-source
    :initarg :geometry-source
    :initform nil)
   (fragment-source
    :initarg :fragment-source
    :initform (error ":fragment-source required"))
   (program-id :initform 0
               :reader shader-program-id)
   (uniform-locations :initform (make-instance 'cache)
                      :documentation "cache uniform-name -> uniform-id")))

(defmethod initialize-instance :after ((shader shader) &rest args)
  (declare (ignore args))
  (flet ((make-shader-source (shader-source)
           (unless (typep shader-source 'shader-source)
             (assert (probe-file shader-source))
             (setf shader-source (make-shader-source :source-file (pathname shader-source))))
           (shader-source-load shader-source)
           (assert (shader-source-file-contents shader-source))
           shader-source))
    (with-slots (vertex-source geometry-source fragment-source) shader
      (setf vertex-source (make-shader-source vertex-source)
            fragment-source (make-shader-source fragment-source))
      (when geometry-source
        (setf geometry-source (make-shader-source geometry-source))))))

(defmethod load-resources ((shader shader))
  ;; compile and link the shader if not already done
  (flet ((assert-no-shader-errors (shader-id)
           (assert (/= 0 shader-id))
           (let ((success (cffi:foreign-alloc :int :initial-element 0)))
             (unwind-protect
                  (progn
                    (%gl:get-shader-iv shader-id :compile-status success)
                    (when (/= 1 (cffi:mem-aref success :int))
                      (error "OpenGl error:~%~A" (gl:get-shader-info-log shader-id))))
               (cffi:foreign-free success))))
         (assert-no-program-errors (program-id)
           (assert (/= 0 program-id))
           (let ((success (cffi:foreign-alloc :int :initial-element 0)))
             (unwind-protect
                  (progn
                    (%gl:get-program-iv program-id :link-status success)
                    (when (/= 1 (cffi:mem-aref success :int))
                      (error "OpenGl error:~%~A" (gl:get-program-info-log program-id))))
               (cffi:foreign-free success)))))
    (with-slots (vertex-source
                 fragment-source
                 geometry-source
                 program-id
                 uniform-locations)
        shader
      (when (= 0 program-id)
        (shader-source-load vertex-source)
        (shader-source-load fragment-source)
        (when geometry-source
          (shader-source-load geometry-source))
        (let ((vertex-source-code (shader-source-file-contents vertex-source))
              (fragment-source-code (shader-source-file-contents fragment-source))
              (geometry-source-code (when geometry-source
                                      (shader-source-file-contents geometry-source)))
              (vertex-shader 0)
              (fragment-shader 0)
              (geometry-shader 0))
          (unwind-protect
               (progn
                 (setf vertex-shader (gl:create-shader :vertex-shader)
                       fragment-shader (gl:create-shader :fragment-shader))
                 (gl:shader-source vertex-shader vertex-source-code)
                 (gl:compile-shader vertex-shader)
                 (assert-no-shader-errors vertex-shader)

                 (gl:shader-source fragment-shader fragment-source-code)
                 (gl:compile-shader fragment-shader)
                 (assert-no-shader-errors fragment-shader)

                 (when geometry-source
                   (setf geometry-shader (gl:create-shader :geometry-shader))
                   (gl:shader-source geometry-shader geometry-source-code)
                   (gl:compile-shader geometry-shader)
                   (assert-no-shader-errors geometry-shader))

                 (handler-case
                     (progn
                       (setf program-id (gl:create-program))
                       (gl:attach-shader program-id vertex-shader)
                       (when geometry-source
                         (gl:attach-shader program-id geometry-shader))
                       (gl:attach-shader program-id fragment-shader)
                       (gl:link-program program-id)
                       (assert-no-program-errors program-id))
                   (error (e)
                     (release-resources shader)
                     (error e))))
            (unless (= 0 vertex-shader)
              (gl:delete-shader vertex-shader))
            (unless (= 0 fragment-shader)
              (gl:delete-shader fragment-shader))
            (unless (= 0 geometry-shader)
              (gl:delete-shader geometry-shader))))))))

(defmethod release-resources ((shader shader))
  (with-slots (program-id uniform-locations) shader
    (unless (= 0 program-id)
      (gl:delete-program program-id)
      (setf program-id 0)
      (clear-cache uniform-locations))))

(defmethod reload ((shader shader))
  "Reload a shader's source. No effect if the shader is not already loaded."
  (unless (= 0 (slot-value shader 'program-id))
    (release-resources shader)
    (load-resources shader)))

(defun set-uniformf (shader uniform-name x &optional y z w)
  (declare (optimize (speed 3))
           (shader shader)
           (string uniform-name)
           (single-float x)
           ((or null single-float) y z w))
  (with-slots (uniform-locations) shader
    (n-uniformf
     (getcache-default uniform-name
                       uniform-locations
                       (n-get-uniform-location (shader-program-id shader) uniform-name))
     x y z w)))

(defun set-uniformi (shader uniform-name x &optional y z w)
  (declare (optimize (speed 3))
           (shader shader)
           (string uniform-name)
           (integer x)
           ((or null integer) y z w))
  (with-slots (uniform-locations) shader
    (n-uniformi
     (getcache-default uniform-name
                       uniform-locations
                       (n-get-uniform-location (shader-program-id shader) uniform-name))
     x y z w)))

(defun set-uniform-matrix-4fv (shader uniform-name matrix &optional (transpose-p t))
  (with-slots (uniform-locations) shader
    (n-uniform-matrix-4fv
     (getcache-default uniform-name
                       uniform-locations
                       (progn
                         (let ((location (n-get-uniform-location (shader-program-id shader) uniform-name)))
                           (declare (fixnum location))
                           (log:debug "shader (shader-program-id shader) cache uniform ~A -> ~A"
                                      uniform-name
                                      location)
                           location)))
     matrix
     transpose-p)))

(defun set-uniform-matrix-1fv (shader uniform-name float-vector)
  (with-slots (uniform-locations) shader
    (sb-sys:with-pinned-objects (float-vector)
      (n-uniform-1fv
       (getcache-default uniform-name
                         uniform-locations
                         (progn
                           (let ((location (n-get-uniform-location (shader-program-id shader) uniform-name)))
                             (declare (fixnum location))
                             (log:debug "shader (shader-program-id shader) cache uniform ~A -> ~A"
                                        uniform-name
                                        location)
                             location)))
       (length float-vector)
       (sb-sys:vector-sap float-vector)))))


;;;; Texture
;; TODO replace texture class
(defclass texture2 ()
  ((texture-id :initform 0 :reader texture-id)
   (texture-src-width :initform nil)
   (texture-src-height :initform nil)
   (texture-parameters :initarg :texture-parameters
                       :initform '(:texture-wrap-s :repeat
                                   :texture-wrap-t :repeat
                                   :texture-min-filter :nearest
                                   :texture-mag-filter :nearest)
                       :documentation "plist passed to (gl:tex-parameter KEY VAL)")))

(defmethod release-resources ((texture texture2))
  (with-slots (texture-id texture-src-width texture-src-height) texture
    (unless (= 0 texture-id)
      (gl:delete-texture texture-id)
      (setf texture-id 0
            texture-src-width nil
            texture-src-height nil))))

(defclass sprite-backed-texture (texture2)
  ((path :initarg :path
         :initform (error ":path required")
         :documentation "filesystem location of image."))
  (:documentation "A textured created from an image on disk."))

(defmethod load-resources ((texture sprite-backed-texture))
  (with-slots ((path-to-texture path)
               texture-id
               texture-src-width
               texture-src-height
               texture-parameters)
      texture
    (when (= 0 texture-id)
      (setf texture-id (gl:gen-texture))
      (handler-case
          (multiple-value-bind
                (img-pointer width height component-count-file component-count-data)
              (cl-soil:load-image path-to-texture :rgba)
            (gl:bind-texture :texture-2d texture-id)
            (unwind-protect
                 (progn
                   (assert (= 4 component-count-file component-count-data))
                   (setf texture-src-width width
                         texture-src-height height)
                   (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte img-pointer :raw t)
                   (gl:generate-mipmap :texture-2d))
              (cl-soil:free-image-data img-pointer))

            (loop :for (gl-texture-param gl-texture-param-val) :on texture-parameters :by #'cddr :do
                 (when (null gl-texture-param-val)
                   (error "texture params list must be a plist: ~A : ~A"
                          texture
                          texture-parameters))
                 (gl:tex-parameter :texture-2d gl-texture-param gl-texture-param-val)))
        (error (e)
          (release-resources texture)
          (gl:bind-texture :texture-2d 0)
          (error e))))))

(defclass texture ()
  ((path-to-texture :initarg :path-to-texture
                    :initform (error ":path-to-texture required"))
   (texture-id :initform 0 :reader texture-id)
   (texture-src-width :initform nil)
   (texture-src-height :initform nil)
   (texture-parameters :initarg :texture-parameters
                       :initform '(:texture-wrap-s :repeat
                                   :texture-wrap-t :repeat
                                   :texture-min-filter :nearest
                                   :texture-mag-filter :nearest)
                       :documentation "plist passed to (gl:tex-parameter KEY VAL)")))

(defun texture-src-width (texture)
  ;; TODO
  ;; (declare (texture texture))
  (with-slots (path-to-texture texture-id texture-src-width)
      texture
    (when (= 0 texture-id)
      (error "texture ~A is not loaded" texture))
    texture-src-width))

(defun texture-src-height (texture)
  ;; TODO
  ;; (declare (texture texture))
  (with-slots (path-to-texture texture-id texture-src-height)
      texture
    (when (= 0 texture-id)
      (error "texture ~A is not loaded" texture))
    texture-src-height))

(defmethod load-resources ((texture texture))
  (with-slots (path-to-texture
               texture-id
               texture-src-width
               texture-src-height
               texture-parameters)
      texture
    (when (= 0 texture-id)
      (setf texture-id (gl:gen-texture))
      (handler-case
          (multiple-value-bind
                (img-pointer width height component-count-file component-count-data)
              (cl-soil:load-image path-to-texture :rgba)
            (gl:bind-texture :texture-2d texture-id)
            (unwind-protect
                 (progn
                   (assert (= 4 component-count-file component-count-data))
                   (setf texture-src-width width
                         texture-src-height height)
                   (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte img-pointer :raw t)
                   (gl:generate-mipmap :texture-2d))
              (cl-soil:free-image-data img-pointer))

            (loop :for (gl-texture-param gl-texture-param-val) :on texture-parameters :by #'cddr :do
                 (when (null gl-texture-param-val)
                   (error "texture params list must be a plist: ~A : ~A"
                          texture
                          texture-parameters))
                 (gl:tex-parameter :texture-2d gl-texture-param gl-texture-param-val)))
        (error (e)
          (release-resources texture)
          (gl:bind-texture :texture-2d 0)
          (error e))))))

(defmethod release-resources ((texture texture))
  (with-slots (texture-id texture-src-width texture-src-height) texture
    (unless (= 0 texture-id)
      (gl:delete-texture texture-id)
      (setf texture-id 0
            texture-src-width nil
            texture-src-height nil))))

;;;; Framebuffer

(defclass framebuffer ()
  ((id :initform -1)
   (texture-id :initform -1
               :reader framebuffer-texture-id)
   (width :initarg :width
          :initform nil
          :documentation "width of the texture attached to this framebuffer.")
   (height :initarg :height
           :initform nil
           :documentation "height of the texture attached to this framebuffer."))
  (:documentation "lisp wrapper for an opengl framebuffer with an attached texture.
If the texture dimensions are not specified the texture will be sized to the configured game resolution."))

(defmethod initialize-instance :after ((fbo framebuffer) &rest args)
  (declare (ignore args))
  (with-slots (width height) fbo
    (destructuring-bind (default-width default-height)
        (or (getconfig 'game-resolution *config*)
            (list 320 180))
      (unless width
        (setf width default-width))
      (unless height
        (setf height default-height)))))

(defmethod load-resources ((framebuffer framebuffer))
  (with-slots (id texture-id width height) framebuffer
    (when (and *gl-context*
               (= -1 id))
      (setf id (gl:gen-framebuffer)
            texture-id (gl:gen-texture))
      (let ((orig-fbo (gl-context-fbo *gl-context*))
            (orig-texture (gl-context-texture *gl-context*)))
        (unwind-protect
             (progn
               (gl-context-use-fbo *gl-context* id)

               (n-bind-texture :texture-2d texture-id)
               (gl:tex-image-2d :texture-2d 0 :rgba width height 0 :rgba :unsigned-byte nil :raw t)
               (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
               (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

               (let ((wrap :clamp-to-border)
                     #+nil
                     (wrap :clamp-to-border)
                     #+nil
                     (wrap :repeat)
                     #+nil
                     (wrap :clamp-to-edge)
                     #+nil
                     (wrap :mirrored-repeat))
                 (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
                 (gl:tex-parameter :texture-2d :texture-wrap-t wrap)
                 ;; border clamp color
                 (gl:tex-parameter :texture-2d :texture-border-color (vector 1.0 1.0 1.0 0.0)))

               (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture-id 0)

               (let ((fbo-status (gl:check-framebuffer-status :framebuffer)))
                 (unless (or (equal fbo-status :framebuffer-complete-oes)
                             (equal fbo-status :framebuffer-complete))
                   (release-resources framebuffer)
                   (error "Error building framebuffer: ~A" fbo-status)))
               (log:debug "Successfully built framebuffer ~A with attached texture ~A"
                          id texture-id))
          (gl-context-use-fbo *gl-context* orig-fbo)
          (gl-bind-texture *gl-context* nil)
          (gl-bind-texture *gl-context* orig-texture))))))

(defmethod release-resources ((framebuffer framebuffer))
  (with-slots (id texture-id) framebuffer
    (unless (= -1 id)
      (when *gl-context*
        ;; NOTE: consing
        (gl:delete-framebuffers (list id))
        (gl:delete-texture texture-id))
      (setf id -1))))

;;;; globals
(defvar *gl-context* nil)

(defvar *shader-cache*
  (getcache-default "shader-cache"
                    *engine-caches*
                    (make-instance 'counting-cache
                                   :on-evict
                                   (lambda (key value)
                                     (declare (ignore key))
                                     (release-resources value)))))

(defvar *texture-cache*
  (getcache-default "texture-cache"
                    *engine-caches*
                    (make-instance 'resource-cache)))

(defvar *framebuffer-cache*
  (getcache-default "framebuffer-cache"
                    *engine-caches*
                    (make-instance 'cache
                                   :on-evict (lambda (dimensions-cons framebuffer-vec)
                                               (declare (ignore dimensions-cons))
                                               (loop :for framebuffer :across framebuffer-vec :do
                                                    (release-resources framebuffer)))))
  "(cons width height) -> (vector framebuffer1 framebuffer2 ...)")

(defvar *identity-matrix*
  (matrix
   1.0 0.0 0.0 0.0
   0.0 1.0 0.0 0.0
   0.0 0.0 1.0 0.0
   0.0 0.0 0.0 1.0))

;; (progn
;;   (format t "fbo cache~%")
;;   (do-cache-with-metadata (*framebuffer-cache* resoluiton vec :next-free-fbo next-free-fbo)
;;     (format t "     ~A -> [next=~A] ~A~%"
;;             resoluiton
;;             next-free-fbo
;;             vec)))

(defun get-tmp-framebuffer (&key width height)
  "Get or create a framebuffer of the specified width and height (game resolution if null).
When done with the tmp framebuffer, call return-tmp-framebuffer to return it to the cache.
framebuffers will be of the specified WIDTHxHEIGHT. If width and height are not specified the game resolution will be used."
  (declare (optimize (speed 3)))
  (destructuring-bind (default-width default-height)
      (or (getconfig 'game-resolution *config*)
          (list 320 180))
    (unless width
      (setf width default-width))
    (unless height
      (setf height default-height)))
  (let* ((dimensions (cons width height))
         (cached-fbos (getcache dimensions *framebuffer-cache*)))
    (declare (dynamic-extent dimensions)
             ((or null vector) cached-fbos))
    (unless cached-fbos
      ;; make sure not to used dynamic-extent dimensions variable for the hash key
      (setf cached-fbos
            (getcache-default (cons width height)
                              *framebuffer-cache*
                              (make-array 0
                                          :adjustable t
                                          :fill-pointer 0
                                          :element-type 'framebuffer))))
    (unless (metadata *framebuffer-cache* dimensions :next-free-fbo)
      (setf (metadata *framebuffer-cache* dimensions :next-free-fbo) 0))
    (when (>= (metadata *framebuffer-cache* dimensions :next-free-fbo)
              (length cached-fbos))
      (let ((fbo (make-instance 'framebuffer
                                :width width
                                :height height)))
        (load-resources fbo)
        (vector-push-extend fbo cached-fbos)))
    (prog1 (elt cached-fbos
           (metadata *framebuffer-cache* dimensions :next-free-fbo) )
      (incf (metadata *framebuffer-cache* dimensions :next-free-fbo)))))

(defun return-tmp-framebuffer (framebuffer)
  (declare (optimize (speed 3)))
  (let* ((dimensions (cons (slot-value framebuffer 'width)
                           (slot-value framebuffer 'height)))
         (cached-fbos (getcache-default dimensions
                                        *framebuffer-cache*
                                        (make-array 0
                                                    :adjustable t
                                                    :fill-pointer 0
                                                    :element-type 'framebuffer)))
         (framebuffer-index (loop :for i :from 0 :below (length cached-fbos) :do
                                 (when (eq (elt cached-fbos i) framebuffer)
                                   (return i))
                               :finally
                                 (log:error "Asked to return framebuffer ~A, but it is not in the framebuffer cache"
                                            framebuffer)
                                 (return-from return-tmp-framebuffer))))
    (declare (dynamic-extent dimensions)
             (vector cached-fbos))

    (decf (metadata *framebuffer-cache* dimensions :next-free-fbo))
    (unless (>= framebuffer-index
                (metadata *framebuffer-cache* dimensions :next-free-fbo))
      (rotatef (elt cached-fbos framebuffer-index)
               (elt cached-fbos
                    (metadata *framebuffer-cache* dimensions :next-free-fbo))))))

(defmacro with-tmp-framebuffer ((framebuffer-name &key width height) &body body)
  (assert (symbolp framebuffer-name))
  (alexandria:once-only (width height)
    `(let ((,framebuffer-name (get-tmp-framebuffer :width ,width :height ,height)))
       (unwind-protect
            (progn ,@body)
         (return-tmp-framebuffer ,framebuffer-name)))))

;;;; FFIs
;;;; cl-opengl wrapper is consing so we'll redefine some non-consing alternatives to use in hot code.

(defmacro %defglfunction ((gl-fn-name lisp-fn-name) return-type &rest arguments)
  (assert (stringp gl-fn-name))
  (assert (symbolp lisp-fn-name))
  `(progn
     (cffi:defcfun (,gl-fn-name ,(alexandria:symbolicate '% lisp-fn-name)) ,return-type
       ,@arguments)
     (defun ,lisp-fn-name (,@(loop :for arg :in arguments :collect (first arg)))
       (prog1
           (,(alexandria:symbolicate '% lisp-fn-name) ,@(loop :for arg :in arguments :collect (first arg)))
         (gl:check-error)))))

(%defglfunction ("glBindVertexArray" n-bind-vertex-array) :void
        (array :unsigned-int))

(%defglfunction ("glUseProgram" n-use-program) :void
  (program :unsigned-int))

(cffi:defcfun ("glGetUniformLocation" %n-get-uniform-location) :int
  (program :unsigned-int)
  (name (:pointer :char)))

(defun n-get-uniform-location (program name)
  (cffi:with-foreign-string (c-str name)
    (prog1
        (%n-get-uniform-location
         program
         c-str)
      (gl:check-error))))

(cffi:defcfun ("glUniform1f" %n-uniform-1f) :void
  (location :int)
  (v0 :float))

(cffi:defcfun ("glUniform2f" %n-uniform-2f) :void
  (location :int)
  (v0 :float)
  (v1 :float))

(cffi:defcfun ("glUniform3f" %n-uniform-3f) :void
  (location :int)
  (v0 :float)
  (v1 :float)
  (v2 :float))

(cffi:defcfun ("glUniform4f" %n-uniform-4f) :void
  (location :int)
  (v0 :float)
  (v1 :float)
  (v2 :float)
  (v3 :float))

(defun n-uniformf (location x &optional y z w)
  (prog1
      (cond
        (w (%n-uniform-4f location (float x) (float y) (float z) (float w)))
        (z (%n-uniform-3f location (float x) (float y) (float z)))
        (y (%n-uniform-2f location (float x) (float y)))
        (x (%n-uniform-1f location (float x))))
    (gl:check-error)))

(cffi:defcfun ("glUniform1i" %n-uniform-1i) :void
  (location :int)
  (v0 :int))

(cffi:defcfun ("glUniform2i" %n-uniform-2i) :void
  (location :int)
  (v0 :int)
  (v1 :int))

(cffi:defcfun ("glUniform3i" %n-uniform-3i) :void
  (location :int)
  (v0 :int)
  (v1 :int)
  (v2 :int))

(cffi:defcfun ("glUniform4i" %n-uniform-4i) :void
  (location :int)
  (v0 :int)
  (v1 :int)
  (v2 :int)
  (v3 :int))

(defun n-uniformi (location x &optional y z w)
  (prog1
      (cond
        (w (%n-uniform-4i location x y z w))
        (z (%n-uniform-3i location x y z))
        (y (%n-uniform-2i location x y))
        (x (%n-uniform-1i location x)))
    (gl:check-error)))

(cffi:defcfun ("glUniformMatrix4fv" %n-uniform-matrix-4fv) :void
  (location :int)
  (count cl-opengl-bindings:sizei)
  (transpose :int)
  (value (:pointer :float)))

(let ((tmp-arr nil))
  (defun n-uniform-matrix-4fv (location matrix &optional (transpose T))
    (declare (optimize (speed 3))
             ((simple-array single-float (16)) matrix))
    (when (null tmp-arr)
      (setf tmp-arr (cffi:foreign-alloc :float :count 16)))
    (loop :for i :from 0 :below (length matrix) :do
         (setf (cffi:mem-aref tmp-arr :float i) (elt matrix i)))
    (prog1
        (%n-uniform-matrix-4fv
         location
         1
         (if (or (null transpose) (equalp 0 transpose))
             0
             1)
         tmp-arr)
      (gl:check-error))))

(%defglfunction ("glUniform1fv" n-uniform-1fv) :void
                (location :int)
                (count cl-opengl-bindings:sizei)
                (value (:pointer :float)))

(%defglfunction ("glActiveTexture" n-active-texture) :void
  (texture %gl:enum))

(%defglfunction ("glBindBuffer" n-bind-buffer) :void
  (target %gl:enum)
  (buffer :unsigned-int))

(%defglfunction ("glBindTexture" n-bind-texture) :void
  (target %gl:enum)
  (texture :unsigned-int))

(cffi:defcfun ("glBufferSubData" %n-buffer-sub-data) :void
  (target %gl:enum)
  (offset cl-opengl-bindings:intptr)
  (size cl-opengl-bindings:sizeiptr)
  (data (:pointer :void)))

(defun n-buffer-sub-data (target array &key (offset 0) (buffer-offset 0)
                                         (size (gl::gl-array-byte-size array)))
  (prog1
      (%n-buffer-sub-data target
                          buffer-offset
                          size
                          (gl::gl-array-pointer-offset array offset))
    (gl:check-error)))

(%defglfunction ("glBufferData" n-buffer-data) :void
  (target %gl:enum)
  (size :int)
  (data (:pointer :void))
  (usage %gl:enum))

(%defglfunction ("glDrawArrays" n-draw-arrays) :void
  (mode %gl:enum)
  (first :int)
  (count cl-opengl-bindings:sizei))

(%defglfunction ("glDrawElements" n-draw-elements) :void
  (mode %gl:enum)
  (count cl-opengl-bindings:sizei)
  (type %gl:enum)
  (indices cl-opengl-bindings::offset-or-pointer))

(%defglfunction ("glDrawArraysInstanced" n-draw-arrays-instanced) :void
  (mode %gl:enum)
  (first :int)
  (count cl-opengl-bindings:sizei)
  (instance-count cl-opengl-bindings:sizei))

(%defglfunction ("glBindFramebuffer" n-bind-framebuffer) :void
                (target %gl:enum)
                (framebuffer :unsigned-int))

;; FIXME: defcfun workaround is incomplete for windows. Will need to be re-thought
#+win32
(progn
  (defun n-bind-vertex-array (array)
    (gl:bind-vertex-array array))

  (defun n-use-program (program-id)
    (gl:use-program program-id))

  (defun n-get-uniform-location (program name)
    (gl:get-uniform-location program name))

  (defun n-uniformf (location x &optional y z w)
    (cond
      (w (gl:uniformf location (float x) (float y) (float z) (float w)))
      (z (gl:uniformf location (float x) (float y) (float z)))
      (y (gl:uniformf location (float x) (float y)))
      (x (gl:uniformf location (float x)))))

  (defun n-uniform-matrix-4fv (location matrix &optional (transpose T))
    (gl:uniform-matrix-4fv location matrix transpose))

  (defun n-active-texture (texture)
    (gl:active-texture texture))

  (defun n-bind-buffer (target buffer)
    (gl:bind-buffer target buffer))

  (defun n-buffer-data (target size data usage)
    (%gl:buffer-data target size data usage))

  (defun n-draw-arrays (mode first count)
    (gl:draw-arrays mode first count))

  (defun n-draw-arrays-instanced (mode first count instance-count)
    (gl:draw-arrays-instanced mode first count instance-count))

  (defun n-draw-elements (mode count type indices)
    (gl:draw-elements mode indices :count count)))
