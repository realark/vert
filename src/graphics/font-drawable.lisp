(in-package :recurse.vert)

(defvar %font-key% 'gl-font)

(defvar %font-buffer-cache%
  (getcache-default "font-buffer-cache"
                    *engine-caches*
                    (make-instance 'counting-cache
                                   :on-evict
                                   (lambda (sprite-key gl-buffers)
                                     (declare (ignore sprite-key))
                                     (destructuring-bind (cached-vao cached-vbo) gl-buffers
                                       (gl:delete-vertex-arrays (list cached-vao))
                                       (gl:delete-buffers (list cached-vbo)))))))

(defstruct glyph-info
  (size (error ":size required") :type kit.math:ivec2)
  ;; Offset from baseline to left/top of glyph
  (bearing (error ":bearing required") :type kit.math:ivec2)
  ;; Horizontal offset to advance to next glyph
  (advance (error ":advance required") :type (integer 0 *))
  ;; location
  (src-x (error "src-x required") :type (integer 0 *)))

(defstruct text-atlas
  (path-to-font (error ":path-to-font required") :type string :read-only t)
  (font-size 72 :type (integer 1 *) :read-only t)
  (beginning 0 :type (integer 0 *) :read-only t)
  (end 128 :type (integer 0 *) :read-only t)
  (texture-id -1 :type integer)
  (width 0 :type (integer 0 *))
  (height 0 :type (integer 0 *))
  (glyph-info #() :type (simple-array glyph-info)))

(defvar *atlas* nil)

(on-engine-start ('atlas-init)
  (let ((atlas (%create-text-atlas
                (or (getconfig 'default-font *config*)
                    (error "No default font specified in ~A" *config*))
                72
                *gl-context*
                :char-code-beginning 0
                :char-code-end 128)))
    (unless (= 0 (text-atlas-texture-id atlas))
      (setf *atlas* atlas))))

(on-engine-stop ('atlas-destroy)
  (when *atlas*
    (gl:delete-texture (text-atlas-texture-id *atlas*))
    (setf *atlas* nil)))

(defun %text-atlas-info-for-char (atlas char)
  (declare (optimize (speed 3))
           (text-atlas atlas)
           (character char))
  (let ((char-index (- (the fixnum (char-code char)) (the fixnum (text-atlas-beginning atlas)))))
    (when (>= char-index (length (text-atlas-glyph-info atlas)))
      (error "~A not in text-atlas ~A" char atlas))
    (elt (text-atlas-glyph-info atlas)
         char-index)))

(defun %create-text-atlas (path-to-font font-size gl-context &key (char-code-beginning 0) (char-code-end 128))
  "Create a texture atlas with chars beginning from CHAR-CODE-BEGINNING below CHAR-CODE-END. Returns GL id to texture."
  (declare (gl-context gl-context)
           (integer char-code-beginning char-code-end))
  (assert (< char-code-beginning char-code-end))
  (labels ((create-font-face (path-to-font font-size)
             "Create a freetype font-face for the given font and font-size."
             (let ((font-face (freetype2:new-face (resource-path path-to-font))))
               (freetype2:set-pixel-sizes font-face 0 font-size)
               font-face))
           (load-freetype-glyphs (font-face char-code-beginning char-code-end)
             "Initialize freetype for specified chars and return (sum-of-all-char-width max-char-height)"
             (loop :with total-width = 0 :and max-height = 0
                :for c :from char-code-beginning :below char-code-end :do
                  (freetype2:load-char font-face c :render)
                  (setf total-width
                        (+ total-width (freetype2::ft-bitmap-width (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face))))
                        max-height (max max-height (freetype2::ft-bitmap-rows (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face)))))
                :finally
                  (return (values total-width max-height))))
           (create-empty-texture (width height)
             "Create an empty opengl texture WIDTHxHEIGHT. Return gl id to texture."
             (gl:active-texture :texture0)
             (let ((texture-id (gl:gen-texture)))
               (gl:bind-texture :texture-2d texture-id)
               (gl:pixel-store :unpack-alignment 1)
               (gl:tex-image-2d :texture-2d 0 :red width height 0 :red :unsigned-byte nil)
               (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
               (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
               ;; Note: If you change :nearest to :linear you will get artifacts
               ;; due to sampling neighboring glyphs. To avid this you will have to add padding.
               (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
               (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
               texture-id)))
    (let ((font-face (create-font-face path-to-font font-size)))
      (multiple-value-bind (atlas-width atlas-height)
          (load-freetype-glyphs font-face char-code-beginning char-code-end)
        (let ((texture-id (create-empty-texture atlas-width atlas-height))
              (glyph-info (make-array (- char-code-end char-code-beginning)
                                      :element-type 'glyph-info)))
          (declare (simple-array glyph-info glyph-info))
          ;; iterate freetype chars, capture glyph info, and write pixels to the texture
          ;; texture is a single-row of all chars in order
          (loop :with x-offset = 0
             :for i :from 0
             :for c :from char-code-beginning :below char-code-end :do
               (freetype2:load-char font-face c :render)
               (let ((glyph-bitmap-width (freetype2::ft-bitmap-width (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face))))
                     (glyph-bitmap-rows (freetype2::ft-bitmap-rows (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face)))))
                 (setf (elt glyph-info i)
                       (make-glyph-info
                        :size (make-array 2
                                          :initial-contents (list glyph-bitmap-width glyph-bitmap-rows)
                                          :element-type '(unsigned-byte 32))
                        :bearing (make-array 2
                                             :initial-contents (list
                                                                ;; FIXME: #\_ char breaks unless we do abs here
                                                                (abs (freetype2::ft-glyphslot-bitmap-left (freetype2::ft-face-glyph font-face)))
                                                                (abs (freetype2::ft-glyphslot-bitmap-top (freetype2::ft-face-glyph font-face))))
                                             :element-type '(unsigned-byte 32))
                        :advance
                        (freetype2::ft-vector-x (freetype2::ft-glyphslot-advance (freetype2::ft-face-glyph font-face)))
                        :src-x x-offset))
                 (gl:tex-sub-image-2d :texture-2d
                                      0 x-offset 0
                                      glyph-bitmap-width glyph-bitmap-rows
                                      :red :unsigned-byte
                                      (freetype2::ft-bitmap-buffer (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face))))
                 (incf x-offset glyph-bitmap-width)))

          (make-text-atlas
           :path-to-font path-to-font
           :font-size font-size
           :texture-id texture-id
           :width atlas-width
           :height atlas-height
           :glyph-info glyph-info))))))

(progn
  (defclass gl-font (draw-component)
    ((font-drawable :initarg :font-drawable
                    :initform (error ":font-drawable required"))
     (shader :initform nil :reader shader)
     (vao :initform 0 :reader vao)
     (vbo :initform 0)
     (vertices-byte-size :initform 0)
     (vertices-pointer-offset :initform 0)
     (vertices :initform nil)))
  (export '(text)))

(defmethod load-resources ((gl-font gl-font) (renderer gl-context))
  (with-slots (font-drawable shader vao vbo vertices vertices-byte-size vertices-pointer-offset)
      gl-font
    (when (= 0 vao)
      (setf shader
            (getcache-default %font-key%
                              *shader-cache*
                              (let ((shader (%create-font-shader)))
                                (load-resources shader renderer)
                                shader)))

      (destructuring-bind (cached-vao cached-vbo)
          (getcache-default gl-font ; %font-key%
                            %font-buffer-cache%
                            (%create-font-buffers))
        (setf vao cached-vao
              vbo cached-vbo))
      (setf vertices (gl:alloc-gl-array :float (* 6 4 (length (text font-drawable))))
            vertices-byte-size (gl::gl-array-byte-size vertices)
            vertices-pointer-offset (gl::gl-array-pointer-offset vertices 0))
      (%set-font-vbo-contents font-drawable renderer))))

(defmethod release-resources ((gl-font gl-font))
  (with-slots (font-drawable shader vao vbo vertices)
      gl-font
    (unless (= 0 vao)
      (remcache %font-key% *shader-cache*)
      (remcache gl-font %font-buffer-cache%)
      (gl:free-gl-array vertices)
      (setf shader nil
            vao 0
            vbo 0
            vertices nil))))

(defun %set-font-vbo-contents (font-drawable renderer)
  (declare (optimize (speed 3)))
  (multiple-value-bind (ix iy iz iw ih)
      (world-dimensions font-drawable)
    (declare (ignore iz)
             (single-float ix iy iw ih))
    (with-slots ((gl-font font-draw-component)) font-drawable
      (with-slots (vao vbo vertices vertices-byte-size vertices-pointer-offset) gl-font
        (loop
           :with scale = (min 1.0
                              (/ iw (%compute-text-width-for-atlas *atlas* (text font-drawable)))
                              (/ ih (text-atlas-height *atlas*)))
           :and x = ix
           :and y = iy
           :with i = 0
           :for char :across (text font-drawable) :do
             (locally (declare (single-float x y scale)
                               (fixnum i))
               (let* ((glyph-info (%text-atlas-info-for-char *atlas* char))
                      (glyph-bearing-x (the fixnum (elt (glyph-info-bearing glyph-info) 0)))
                      (glyph-bearing-y (elt (glyph-info-bearing glyph-info) 1))
                      (glyph-size-x (elt (glyph-info-size glyph-info) 0))
                      (glyph-size-y (elt (glyph-info-size glyph-info) 1))
                      (xpos (+ x (the single-float (* glyph-bearing-x scale))))
                      (ypos (+ y (the single-float (* (- (the fixnum (text-atlas-height *atlas*)) (the fixnum glyph-bearing-y)) scale))))
                      (w  (* glyph-size-x scale))
                      (h (* glyph-size-y scale))
                      (src-x (/ (float (the fixnum (glyph-info-src-x glyph-info)))
                                (float (the fixnum (text-atlas-width *atlas*)))))
                      (src-y 0.0)
                      (src-w (/ (float (the fixnum glyph-size-x))
                                (float (the fixnum (text-atlas-width *atlas*)))))
                      (src-h (/ (float (the fixnum glyph-size-y))
                                (float (the fixnum (text-atlas-height *atlas*))))))
                 (macrolet ((set-vertices-data (&rest data)
                              `(progn
                                 ,@(loop :for val :in data :collect
                                        `(setf (cffi:mem-aref (gl::gl-array-pointer vertices) :float i)
                                               ,val
                                               i (+ i 1))))))
                   (set-vertices-data
                    ;; -- second triangle
                    ;; top left
                    xpos         ypos        src-x  src-y
                    ;; bottom left
                    xpos         (+ ypos h)  src-x  (+ src-y src-h)
                    ;; bottom right
                    (+ xpos w)   (+ ypos h)  (+ src-x src-w)  (+ src-y src-h)
                    ;; -- first triangle
                    ;; bottom right
                    (+ xpos w)   (+ ypos h)  (+ src-x src-w)  (+ src-y src-h)
                    ;; top right
                    (+ xpos w)   ypos        (+ src-x src-w) src-y
                    ;; top left
                    xpos         ypos        src-x  src-y))

                 ;; Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
                 ;; Bitshift by 6 to get value in pixels (2^6 = 64 (divide amount of 1/64th pixels by 64 to get amount of pixels))
                 (incf x (* (ash (the fixnum (glyph-info-advance glyph-info)) -6) scale)))))
        (unless (= 0 (the fixnum vao))
          (gl-use-vao renderer vao)
          (n-bind-buffer :array-buffer vbo)
          (n-buffer-data :array-buffer vertices-byte-size (gl::gl-array-pointer vertices) :dynamic-draw))))))

(defmethod render ((gl-font gl-font) update-percent (camera simple-camera) (renderer gl-context))
  (declare (optimize (speed 3)))
  (when *atlas*
    (with-slots (font-drawable shader vao vbo vertices vertices-byte-size vertices-pointer-offset)
        gl-font
      (with-accessors ((color color) (text text))
          font-drawable
        (gl-use-shader renderer shader)

        (set-uniform-matrix-4fv shader "projection"
                                (interpolated-world-projection-matrix camera update-percent)
                                nil)

        (if color
            (set-uniformf shader "textColor"
                          (r color)
                          (g color)
                          (b color)
                          (a color))
            (set-uniformf shader "textColor"
                          1.0 1.0 1.0 1.0))

        (when *atlas*
          (gl-use-vao renderer vao)

          (n-active-texture :texture0)
          (gl-bind-texture renderer nil)
          (n-bind-texture :texture-2d (text-atlas-texture-id *atlas*))

          (when (slot-value font-drawable 'dirty-p)
            (local-to-world-matrix font-drawable)
            (%set-font-vbo-contents font-drawable renderer))
          (n-draw-arrays :triangles 0 (* 6 (length (the vector text)))))))))

(defun %compute-text-width-for-atlas (atlas text)
  (declare (optimize (speed 3))
           (string text)
           (text-atlas atlas))
  (loop :with width = 0
     :for char :across text :do
       (let* ((glyph (%text-atlas-info-for-char atlas char)))
         (declare (fixnum width))
         ;; Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
         (setf width (+ width (ash (the fixnum (glyph-info-advance glyph)) -6))))
     :finally (return width)))

(defun %create-font-shader ()
  (make-instance 'shader
                 :vertex-source
                 (get-builtin-shader-source 'font-shader.vert)
                 :fragment-source
                 (get-builtin-shader-source 'font-shader.frag)))

(defun %create-font-buffers ()
  "set up the vao, vbo, and vertex array for rendering the font"
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer)))

    (gl-use-vao *gl-context* vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:vertex-attrib-pointer 0 4 :float 0 (* 4 (cffi:foreign-type-size :float)) 0)

    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)

    (list vao vbo)))

;;;; Font-Drawable (Game object drawn with a gl-font draw-component)

@export
(defclass font-drawable (drawable obb)
  ;; duplicate slots to use for defaults of cached characters
  ((draw-component :initform nil)
   (font-draw-component :initform nil)
   (text :initarg :text
         :initform (error ":text required")
         :accessor text)
   (path-to-font :initarg :path-to-font
                 :initform (or (getconfig 'default-font *config*)
                               (error "No default font specified in ~A" *config*))
                 :reader path-to-font
                 :documentation "Path to font file.")
   (font-size :initform 72
              :initarg :font-size
              :reader font-size
              :documentation "Pixel depth of the font."))
  (:documentation "A drawable which loads pixels from a font and user-defined text"))

(defmethod (setf text) :around (new-text (font-drawable font-drawable))
  (let ((old-text (text font-drawable)))
    (prog1 (call-next-method new-text font-drawable)
      (unless (equal old-text (text font-drawable))
        (when *gl-context*
          (with-slots (font-draw-component) font-drawable
            (with-slots (vertices vertices-byte-size vertices-pointer-offset) font-draw-component
              (when (or (null vertices)
                        (> (* 6 4 (length (text font-drawable))) (gl::gl-array-size vertices)))
                (when vertices
                  (gl:free-gl-array vertices)
                  (setf vertices nil))
                (setf vertices (gl:alloc-gl-array :float (* 6 4 (length (text font-drawable))))
                      vertices-byte-size (gl::gl-array-byte-size vertices)
                      vertices-pointer-offset (gl::gl-array-pointer-offset vertices 0)))
              (when (and *atlas* *gl-context*)
                (%set-font-vbo-contents font-drawable *gl-context*)))))))))

(defmethod initialize-instance :after ((font-drawable font-drawable) &rest args)
  (declare (ignore args))
  (with-slots (font-draw-component) font-drawable
    (setf font-draw-component (make-instance 'gl-font
                                             :font-drawable font-drawable)
          (draw-component font-drawable) font-draw-component)))

(defmethod load-resources ((font-drawable font-drawable) rendering-context)
  (with-slots (font-draw-component)
      font-drawable
    (load-resources font-draw-component rendering-context)
    (load-resources (draw-component font-drawable) rendering-context)))

(defmethod release-resources ((font-drawable font-drawable))
  (with-slots (font-draw-component) font-drawable
    (release-resources font-draw-component)
    (release-resources (draw-component font-drawable))))

(defun font-dimensions (font-drawable)
  (declare (font-drawable font-drawable))
  (with-slots ((gl-font font-draw-component)) font-drawable
    (if *atlas*
        (values (%compute-text-width-for-atlas *atlas* (text font-drawable))
                (%text-atlas-info-for-char *atlas* #\y))
        (values 100 10))))
