(in-package :recurse.vert)

(defclass font-drawable (sdl-texture-drawable)
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

(defmethod initialize-instance :after ((drawable font-drawable) &key color)
  (setf (color-mod drawable) color)
  (setf (path-to-font drawable) (path-to-font drawable)))

(defun font-dimensions (font-drawable)
  "Return a value list of suggested pixel width and height of FONT-DRAWABLE based on the font size."
  (with-slots (texture width height) font-drawable
    (if texture
        (values
         (sdl2:texture-width texture)
         (sdl2:texture-height texture))
        (values width height))))

(defmethod (setf path-to-font) :before (value (font-drawable font-drawable))
  (unless (probe-file value)
    (error (format nil "file not found: \"~A\"" value))))

(defmethod (setf text) :around (new-value (font-drawable font-drawable))
  (unless (equal (text font-drawable) new-value)
    (release-resources font-drawable)
    (call-next-method new-value font-drawable)))

(defmethod create-sdl-texture ((drawable font-drawable) (renderer sdl2-ffi:sdl-renderer))
  (with-slots (path-to-font font-size text) drawable
    (let* ((font (sdl2-ttf:open-font path-to-font font-size))
           (surf (sdl2-ttf:render-text-blended font text 255 255 255 0))
           (texture (sdl2:create-texture-from-surface renderer surf)))
      (sdl2-ttf:close-font font)
      (sdl2:free-surface surf)
      texture)))
