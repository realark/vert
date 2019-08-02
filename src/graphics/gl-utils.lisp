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
  "Stores and sets options for the underlying opengl context with various optimizations.
For example, the "
  (wrapper (error ":wrapper required") :type sdl2-ffi::sdl-glcontext)
  (shader nil :type (or null shader))
  (texture nil :type (or null texture))
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

(defun gl-context-clear-all (gl-context)
  (setf (gl-context-shader gl-context) nil
        (gl-context-texture gl-context) nil
        (gl-context-vao gl-context) 0))

;;;; Shader
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct shader-source
    (source-file (error ":source-file required"))
    (file-contents nil :type (or null string)))

  (defun shader-source-load (shader-source)
    (declare (shader-source shader-source))
    (if (shader-source-source-file shader-source)
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

(defmethod load-resources ((shader shader) context)
  (declare (ignore context))
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
    (load-resources shader t)))

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

(defun set-uniform-matrix-4fv (shader uniform-name matrix &optional (transpose-p t))
  (with-slots (uniform-locations) shader
    (n-uniform-matrix-4fv
     (getcache-default uniform-name
                       uniform-locations
                       (n-get-uniform-location (shader-program-id shader) uniform-name))
     matrix
     transpose-p)))

;;;; Texture
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
  (declare (texture texture))
  (with-slots (path-to-texture texture-id texture-src-width)
      texture
    (when (= 0 texture-id)
      (error "texture ~A is not loaded" path-to-texture))
    texture-src-width))

(defun texture-src-height (texture)
  (declare (texture texture))
  (with-slots (path-to-texture texture-id texture-src-height)
      texture
    (when (= 0 texture-id)
      (error "texture ~A is not loaded" path-to-texture))
    texture-src-height))

(defmethod load-resources ((texture texture) (renderer gl-context))
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

(%defglfunction ("glDrawArrays" n-draw-arrays) :void
  (mode %gl:enum)
  (first :int)
  (count cl-opengl-bindings:sizei))

(%defglfunction ("glDrawElements" n-draw-elements) :void
  (mode %gl:enum)
  (count cl-opengl-bindings:sizei)
  (type %gl:enum)
  (indices cl-opengl-bindings::offset-or-pointer))
