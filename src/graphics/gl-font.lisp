(in-package :recurse.vert)

;; TODO: rename %glyph?
(defstruct glyph
  ;; ID handle of the glyph texture
  (texture-id (error ":texture-id required") :type integer)
  ;; Size of glyph
  (size (error ":size required") :type kit.math:ivec2)
  ;; Offset from baseline to left/top of glyph
  (bearing (error ":bearing required") :type kit.math:ivec2)
  ;; Horizontal offset to advance to next glyph
  (advance (error ":advance required") :type (integer 0 *)))

(defvar %font-key% (make-instance 'standard-object))

(progn
  (defclass gl-font (gl-drawable)
    ((text :initarg :text
           :initform (error ":text required")
           :accessor text)
     (path-to-font :initarg :path-to-font
                   :initform "fonts/liberation_sans/LiberationSans-Regular.ttf"
                   :reader path-to-font
                   :documentation "Path to font file.")
     (font-size :initform 72
                :initarg :font-size
                :documentation "Pixel depth of the font.")
     (color :initarg :color
            :initform nil
            :documentation "Color of the text."
            :accessor color)
     (shader-cache :initarg :shader-cache
                   :initform *shader-cache*)
     (shader :initform nil :reader shader)
     (glyph-cache :initarg :glyph-cache
                  :initform
                  (getcache-default
                   "glyph-cache"
                   *engine-caches*
                   (make-instance 'counting-cache
                                  :on-evict
                                  (lambda (font-size glyph-map)
                                    (declare (ignore font-size))
                                    (%free-glyph-map glyph-map))))
                  :documentation "cache font-size->glyph-cache")
     (glyph-map :initform nil)
     (buffer-cache :initarg :buffer-cache
                   :initform
                   (getcache-default "font-buffer-cache"
                                     *engine-caches*
                                     (make-instance 'counting-cache
                                                    :on-evict
                                                    (lambda (sprite-key gl-buffers)
                                                      (declare (ignore sprite-key))
                                                      (destructuring-bind (cached-vao cached-vbo cached-vertices) gl-buffers
                                                        (gl:delete-vertex-arrays (list cached-vao))
                                                        (gl:delete-buffers (list cached-vbo))
                                                        (gl:free-gl-array cached-vertices))))))
     (vao :initform 0 :reader vao)
     (vbo :initform 0)
     (vertices-byte-size :initform 0)
     (vertices-pointer-offset :initform 0)
     (vertices :initform nil)))
  (export '(text)))

(defun font-dimensions (gl-font)
  (declare (gl-font gl-font))
  (with-slots (text glyph-cache font-size) gl-font
    (let ((glyph-map (getcache font-size glyph-cache)))
      (if glyph-map
          (values (%compute-text-width text glyph-map)
                  (elt (glyph-size (gethash (char-code #\y) glyph-map)) 1))
          (values 100 10)))))

(defmethod load-resources ((gl-font gl-font) (renderer gl-context))
  (with-slots (shader-cache shader font-size glyph-map glyph-cache vao vbo vertices vertices-byte-size vertices-pointer-offset buffer-cache)
      gl-font
    (setf shader
          (getcache-default %font-key%
                            shader-cache
                            (let ((shader (%create-font-shader)))
                              (load-resources shader renderer)
                              shader)))

    (setf glyph-map (getcache-default font-size
                                      glyph-cache
                                      (%init-glyph-map gl-font)))

    (destructuring-bind (cached-vao cached-vbo cached-vertices)
        (getcache-default %font-key%
                          buffer-cache
                          (%create-font-buffers))
      (setf vao cached-vao
            vbo cached-vbo
            vertices cached-vertices
            vertices-byte-size (gl::gl-array-byte-size vertices)
            vertices-pointer-offset (gl::gl-array-pointer-offset vertices 0)))))

(defmethod release-resources ((gl-font gl-font))
  (with-slots (shader-cache shader font-size glyph-cache glyph-map buffer-cache vao vbo)
      gl-font
    (remcache %font-key% shader-cache)
    (remcache font-size glyph-cache)
    (remcache %font-key% buffer-cache)
    (setf shader nil
          glyph-map nil
          vao 0
          vbo 0)))

(defmethod render ((gl-font gl-font) update-percent (camera simple-camera) (renderer gl-context))
    (declare (optimize (speed 3)))
    (with-slots (color shader glyph-map vao vbo text vertices vertices-byte-size vertices-pointer-offset)
        gl-font

      (gl-use-shader renderer shader)

      (set-uniform-matrix-4fv shader
                              "projection"
                              (interpolated-world-projection-matrix camera update-percent)
                              nil)

      (if color
          (set-uniformf shader
                        "textColor"
                        (r color)
                        (g color)
                        (b color)
                        (a color))
          (set-uniformf shader
                        "textColor"
                        1.0 1.0 1.0 1.0))
      (n-active-texture :texture0)
      (gl-use-vao renderer vao)
      (n-bind-buffer :array-buffer vbo)


      (multiple-value-bind (ix iy iz iw ih)
          (world-dimensions gl-font)
        (loop
           ;; width = text-width * scale
           :with scale = (min 1.0
                              (/ iw (%compute-text-width text glyph-map))
                              (/ ih (elt (glyph-size (gethash (char-code #\y) glyph-map)) 1)))
           :and x = ix
           :and y = iy
           ;; Using for offset #\H because its bearing touches the top of the glyph space
           :and h-bearing-y = (elt (glyph-bearing (gethash (char-code #\H) glyph-map)) 1)
           :for char :across text :do
             (let* ((glyph (gethash (char-code char) glyph-map))
                    (glyph-bearing-x (elt (glyph-bearing glyph) 0))
                    (glyph-bearing-y (elt (glyph-bearing glyph) 1))
                    (glyph-size-x (elt (glyph-size glyph) 0))
                    (glyph-size-y (elt (glyph-size glyph) 1))
                    (xpos (+ x (* glyph-bearing-x scale)))
                    (ypos (+ y (* (- h-bearing-y glyph-bearing-y) scale)) )
                    (w (* glyph-size-x scale))
                    (h (* glyph-size-y scale)))
               (macrolet ((set-vertices-data (&rest data)
                            `(progn
                               ,@(loop :for val :in data
                                    :for i :from 0 :collect
                                      `(setf (cffi:mem-aref (gl::gl-array-pointer vertices)
                                                            :float
                                                            ,i)
                                             ,val)))))
                 (set-vertices-data
                  ;; bottom right
                  (+ xpos w)   (+ ypos h)  1.0  1.0
                  ;; top right
                  (+ xpos w)   ypos        1.0  0.0
                  ;; top left
                  xpos         ypos        0.0  0.0
                  ;; bottom left
                  xpos         (+ ypos h)  0.0  1.0))

               ;; TODO: it would probably be a lot faster to generate and cache the texture just once
               (gl-bind-texture renderer nil)
               (n-bind-texture :texture-2d (glyph-texture-id glyph))
               (%n-buffer-sub-data :array-buffer
                                   0
                                   vertices-byte-size
                                   vertices-pointer-offset)
               (n-draw-arrays :triangle-fan 0 4)

               ;; Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
               ;; Bitshift by 6 to get value in pixels (2^6 = 64 (divide amount of 1/64th pixels by 64 to get amount of pixels))
               (incf x (* (ash (glyph-advance glyph) -6) scale)))))))

(defun %compute-text-width (text glyph-map)
  (loop :with width = 0
     :for char :across text :do
       (let* ((glyph (gethash (char-code char) glyph-map)))
         ;; Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
         (incf width (ash (glyph-advance glyph) -6)))
     :finally (return width)))

(defun %create-font-shader ()
  (let* ((vertex-shader-source
          "#version 330 core
layout (location = 0) in vec4 vertex; // <vec2 pos, vec2 tex>
out vec2 TexCoords;

uniform mat4 projection;

void main()
{
    gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
    TexCoords = vertex.zw;
}")
         (fragment-shader-source
          "#version 330 core
in vec2 TexCoords;
out vec4 color;

uniform sampler2D text;
uniform vec4 textColor;

void main()
{
    vec4 sampled = vec4(1.0, 1.0, 1.0, texture(text, TexCoords).r);
    color = textColor * sampled;
}")
         (font-shader (make-instance 'shader
                                     :vertex-source vertex-shader-source
                                     :fragment-source fragment-shader-source)))
    font-shader))

(defun %create-font-buffers ()
  "set up the vao, vbo, and vertex array for rendering the font"
  (let ((vao (gl:gen-vertex-array))
        (vbo (gl:gen-buffer)))

    (gl-use-vao *gl-context* vao)

    (gl:bind-buffer :array-buffer vbo)
    (%gl::buffer-data :array-buffer (* (cffi:foreign-type-size :float) 6 4) (cffi:null-pointer) :dynamic-draw)

    ;; position
    (gl:vertex-attrib-pointer 0 4 :float 0 (* 4 (cffi:foreign-type-size :float)) 0)
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer 0)

    (list vao
          vbo
          (alloc-gl-array :float
                          ;; positions
                          0.0  0.0  0.0  0.0
                          0.0  0.0  0.0  0.0
                          0.0  0.0  0.0  0.0
                          0.0  0.0  0.0  0.0))))

(defun %init-glyph-map (gl-font)
  (declare (gl-font gl-font))
  (with-slots (path-to-font)
      gl-font
    (let ((font-face (freetype2:new-face (resource-path path-to-font))))
      (freetype2:set-pixel-sizes font-face 0 48)
      (gl:pixel-store :unpack-alignment 1)
      ;; TODO: configurable chars
      (loop :with glyph-map = (make-hash-table)
         :for c :from 0 :below 128 :do
           (freetype2:load-char font-face c :render)

           (let ((texture (gl:gen-texture)))
             (gl:bind-texture :texture-2d texture)
             (gl:tex-image-2d
              :texture-2d
              0
              :red
              (freetype2::ft-bitmap-width (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face)))
              (freetype2::ft-bitmap-rows (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face)))
              0
              :red
              :unsigned-byte
              (freetype2::ft-bitmap-buffer (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face))))
             (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
             (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
             (gl:tex-parameter :texture-2d :texture-min-filter :linear)
             (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

             (setf (gethash c glyph-map)
                   (make-glyph
                    :texture-id texture
                    :size (make-array 2
                                      :initial-contents (list
                                                         (freetype2::ft-bitmap-width (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face)))
                                                         (freetype2::ft-bitmap-rows (freetype2::ft-glyphslot-bitmap (freetype2::ft-face-glyph font-face))))
                                      :element-type '(unsigned-byte 32))
                    :bearing (make-array 2
                                         :initial-contents (list
                                                            ;; FIXME: #\_ char breaks unless we do abs here
                                                            (abs (freetype2::ft-glyphslot-bitmap-left (freetype2::ft-face-glyph font-face)))
                                                            (abs (freetype2::ft-glyphslot-bitmap-top (freetype2::ft-face-glyph font-face))))
                                         :element-type '(unsigned-byte 32))
                    :advance
                    (freetype2::ft-vector-x (freetype2::ft-glyphslot-advance (freetype2::ft-face-glyph font-face))))))
         :finally
           (gl:bind-texture :texture-2d 0)
           (return glyph-map)))))

(defun %free-glyph-map (glyph-map)
  (loop :for c :being :the hash-keys :of glyph-map
     :using (hash-value glyph) :do
       (gl:delete-texture (glyph-texture-id glyph))))
