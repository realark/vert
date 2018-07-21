(in-package :recurse.vert)

@export
(defun make-sprite-source (x y w h)
  (sdl2:make-rect x y w h))
@export
(defun sprite-source-x (sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (sdl2:rect-x sprite-source))
@export
(defun (setf sprite-source-x) (new-value sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (setf (sdl2:rect-x sprite-source) new-value))
@export
(defun sprite-source-y (sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (sdl2:rect-y sprite-source))
@export
(defun (setf sprite-source-y) (new-value sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (setf (sdl2:rect-y sprite-source) new-value))
@export
(defun sprite-source-w (sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (sdl2:rect-width sprite-source))
@export
(defun (setf sprite-source-w) (new-value sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (setf (sdl2:rect-width sprite-source) new-value))
@export
(defun sprite-source-h (sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (sdl2:rect-height sprite-source))
@export
(defun (setf sprite-source-h) (new-value sprite-source)
  (declare (sdl2-ffi:sdl-rect sprite-source))
  (setf (sdl2:rect-height sprite-source) new-value))

(defclass static-sprite (sdl-texture-drawable)
  ((path-to-image :initarg :path-to-image
                  :initform (error ":path-to-image must be specified")
                  :accessor path-to-image
                  :documentation "Path to image file.")
   (sdl-source-rectangle :initform nil
                         :initarg :sprite-source
                         :documentation "A sprite-source which specifies which portion of the image to render"))
  (:documentation "A game-object which loads pixels from an image resource for rendering."))

@export
(defgeneric sprite-source (static-sprite)
  (:documentation "Source section of the sprite to render.")
  (:method ((static-sprite static-sprite))
    (slot-value static-sprite 'sdl-source-rectangle)))

(defmethod (setf sprite-source) ((new-source sdl2-ffi:sdl-rect) (static-sprite static-sprite))
  (setf (slot-value static-sprite 'sdl-source-rectangle) new-source))

(defmethod initialize-instance :after ((game-object static-sprite) &rest args)
  (declare (ignore args))
  (setf (path-to-image game-object) (path-to-image game-object)))

(defmethod (setf path-to-image) :before (value (static-sprite static-sprite))
  (declare (optimize (speed 3) (space 3)))
  ;; probing file is too expensive for animated sprites
  ;; (unless (probe-file value)
  ;;   (error (format nil "file not found: \"~A\"" value)))
  (setf (texture-cache-key static-sprite) value))

(defmethod create-sdl-texture ((static-sprite static-sprite) (renderer sdl2-ffi:sdl-renderer))
  (let* ((surf (sdl2-image:load-image (path-to-image static-sprite)))
         (texture (sdl2:create-texture-from-surface renderer surf)))
    (sdl2:free-surface surf)
    texture))

@export
(defgeneric flip (static-sprite direction)
  (:documentation "Toggle STATIC-SPRITE in the given DIRECTION.
A DIRECTION of :NONE will clear all flips")
  (:method ((drawable static-sprite) (direction symbol))
    (with-slots (flip-list) drawable
      (ecase direction
        (:none (setf flip-list (list)))
        ((:horizontal :vertical)
         (if (list-contains-p flip-list direction)
             (setf flip-list (delete direction flip-list))
             (push :horizontal flip-list)))))))
