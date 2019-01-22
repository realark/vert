(in-package :recurse.vert)

(defclass uncached-font-drawable (sdl-texture-drawable)
  ((path-to-font :initarg :path-to-font
                 ;; TODO configurable font and default font
                 :initform (resource-path "fonts/liberation_sans/LiberationSans-Regular.ttf")
                 ;; :initform (error ":path-to-font must be specified")
                 :accessor path-to-font
                 :documentation "Path to font file.")
   (font-size :initarg :font-size
              :initform 72
              :accessor font-size
              ;; TODO: Compute this based on the size of the drawable
              :documentation "Pixel depth of the font based on 72DPI.")
   (text :initarg :text
         :initform (error ":text must be specified")
         :accessor text))
  (:documentation "A drawable which loads pixels from a font and user-defined text"))

(defmethod initialize-instance :after ((drawable uncached-font-drawable) &key color)
  (setf (color-mod drawable) color)
  (setf (path-to-font drawable) (path-to-font drawable)))

(defmethod (setf path-to-font) :before (value (uncached-font-drawable uncached-font-drawable))
  (unless (probe-file value)
    (error (format nil "file not found: \"~A\"" value))))

(defmethod (setf text) :around (new-value (uncached-font-drawable uncached-font-drawable))
  (unless (equal (text uncached-font-drawable) new-value)
    (release-resources uncached-font-drawable)
    (call-next-method new-value uncached-font-drawable)))

(defmethod create-sdl-texture ((drawable uncached-font-drawable) (renderer sdl2-ffi:sdl-renderer))
  (with-slots (path-to-font font-size text) drawable
    (let* ((font (sdl2-ttf:open-font path-to-font font-size))
           (surf (sdl2-ttf:render-text-blended font text 255 255 255 0))
           (texture (sdl2:create-texture-from-surface renderer surf)))
      (sdl2-ttf:close-font font)
      (sdl2:free-surface surf)
      texture)))

;; TODO: directly cache the textures themselves instead of historical uncached-font-drawable cruft
(unless (get-registered-cache *memory-manager* "text-cache")
  (register-cache *memory-manager* "text-cache"
                  (make-instance 'cache :on-evict (lambda (font-size font-cache)
                                                    (declare (ignore font-size))
                                                    (clear-cache font-cache))
                                        :test #'equalp)))

@export
(defclass font-drawable (sdl-texture-drawable)
  ((characters :initform (make-array 0 :adjustable T :fill-pointer 0))
   ;; duplicate slots to use for defaults of cached characters
   (path-to-font :initarg :path-to-font
                 :initform (resource-path "fonts/liberation_sans/LiberationSans-Regular.ttf")
                 :accessor path-to-font
                 :documentation "Path to font file.")
   (font-size :initarg :font-size
              :initform 72
              :accessor font-size
              :documentation "Pixel depth of the font based on 72DPI.")
   (text :initarg :text
         :initform (error ":text must be specified")
         :accessor text))
  (:documentation "A drawable which loads pixels from a font and user-defined text"))

(defmethod initialize-instance :after ((drawable font-drawable) &key color)
  (setf (color-mod drawable) color)
  (let ((txt (text drawable)))
    ;; Ensure text setf :before will be run no matter what the initial text is
    (setf (text drawable) ""
          (text drawable) txt)))

(defun font-dimensions (font-drawable)
  "Return a value list of suggested pixel width and height of FONT-DRAWABLE based on the font size."
  (with-slots (width height characters) font-drawable
    (let ((texture (when (> (length characters) 0)
                     (slot-value (elt characters 0) 'texture))))
      (if texture
          (values
           (* (length characters)
              (sdl2:texture-width texture))
           (sdl2:texture-height texture))
          (values width height)))))

(defmethod (setf text) :before (new-text (font-drawable font-drawable))
  (with-slots ((old-text text)
               characters
               color-mod
               path-to-font
               font-size)
      font-drawable
    (unless (equal old-text new-text)
      (setf (fill-pointer characters) 0)
      (when new-text
        (loop :for char :across new-text :do
          (vector-push-extend (getcache-default char
                                                (getcache-default font-size
                                                                  (get-registered-cache *memory-manager* "text-cache")
                                                                  (make-instance 'cache :on-evict (lambda (char char-drawable)
                                                                                                    (declare (ignore char))
                                                                                                    (release-resources char-drawable))
                                                                                        :test #'eql))
                                                (make-instance 'uncached-font-drawable
                                                               :color color-mod
                                                               :path-to-font path-to-font
                                                               :text (format nil "~c" char)))
                              characters))))))

(defmethod load-resources ((font-drawable font-drawable) (renderer sdl2-ffi:sdl-renderer))
  (with-slots (characters) font-drawable
    (when characters
      (loop :for char-drawable :across characters :do
        (load-resources char-drawable renderer)))))

(defmethod render ((font-drawable font-drawable) update-percent camera rendering-context)
  (with-slots (characters sdl-rectangle last-positions) font-drawable
    (loop :with n = 0
          :and char-width = (/ (width font-drawable) (length characters))
          :and interpolated-width = (round (/ (sdl2:rect-width sdl-rectangle) (length characters)))
          :for char :across characters :do
            (with-slots ((char-last-positions last-positions)) char
              (setf (y char) (y font-drawable)
                    (height char) (height font-drawable)
                    (width char) char-width
                    (x char) (+ (x font-drawable) (* n char-width))
                    (color-mod char) (color-mod font-drawable)
                    (elt char-last-positions 0) (+ (elt last-positions 0) (* n interpolated-width))
                    (elt char-last-positions 1) (elt last-positions 1)
                    n (+ n 1))
              (render char update-percent camera rendering-context)))))
