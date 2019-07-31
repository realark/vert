(in-package :recurse.vert)

@export
(defclass font-drawable (drawable obb)
  ;; duplicate slots to use for defaults of cached characters
  ((draw-component :initform nil)
   (font-draw-component :initform nil)
   (text :initarg :text
         :initform (error ":text required")
         :accessor text)
   (path-to-font :initarg :path-to-font
                 :initform "fonts/liberation_sans/LiberationSans-Regular.ttf"
                 :reader path-to-font
                 :documentation "Path to font file.")
   (font-size :initform 72
              :initarg :font-size
              :reader font-size
              :documentation "Pixel depth of the font."))
  (:documentation "A drawable which loads pixels from a font and user-defined text"))

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
    (let ((glyph-map (getcache (font-size font-drawable) %glyph-cache%)))
      (if glyph-map
          (values (%compute-text-width (text font-drawable) glyph-map)
                  (elt (glyph-size (gethash (char-code #\y) glyph-map)) 1))
          (values 100 10)))))
