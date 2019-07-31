(in-package :recurse.vert)

(progn
  (defclass static-sprite (drawable obb)
    ((draw-component :initform nil)
     ;; static-sprite needs a sprite-draw-component to get texture width.
     ;; it's possible for subclasses to replace the draw-component so we'll store the sprite
     ;; component in a separate slot to be safe.
     (sprite-draw-component :initform nil)
     (path-to-sprite :initarg :path-to-sprite
                     :initform (error ":path-to-sprite must be specified")
                     :accessor path-to-sprite
                     :documentation "Path to the sprite file.")
     (color-maps :initform nil
                 :documentation "Before color mod is applied, allow mapping src colors to dest colors")
     (sprite-source :initarg :sprite-source
                    :initform nil
                    :documentation "Rectangle subset of the sprite to render.
Nil to render the entire sprite."
                    :accessor sprite-source)
     (sprite-source-flip-vector :initform
                                (make-array 2
                                            :initial-contents '(1.0 1.0)
                                            :element-type 'single-float)
                                :reader sprite-source-flip-vector)
     (wrap-width :initform nil
                 :initarg :wrap-width)
     (wrap-height :initform nil
                  :initarg :wrap-height)
     (flip-list :initform (list)
                :accessor flip-list))
    (:documentation "A game-object which loads pixels from an image resource for rendering."))

  (export '(sprite-source sprite-source-x sprite-source-y sprite-source-width sprite-source-height)))

(defmethod initialize-instance :after ((sprite static-sprite) &rest args)
  (declare (ignore args))
  (with-slots (sprite-draw-component
               path-to-sprite
               sprite-source
               wrap-width
               wrap-height)
      sprite
    (setf sprite-draw-component (make-instance 'gl-sprite :static-sprite sprite)
          (draw-component sprite) sprite-draw-component)
    (when (and (or wrap-width wrap-height)
               sprite-source)
      (error "using wrap-width/height with sprite-source not supported. ~A" sprite))))

(defmethod load-resources ((sprite static-sprite) rendering-context)
  (with-slots (sprite-draw-component path-to-sprite wrap-width wrap-height)
      sprite
    (load-resources sprite-draw-component rendering-context)
    (load-resources (draw-component sprite) rendering-context)

    (with-accessors ((sprite-source sprite-source)
                     (width width) (height height))
        sprite
      (with-accessors ((texture gl-sprite-texture))
          sprite-draw-component
        (when (or wrap-width wrap-height)
          (setf sprite-source
                (make-sprite-source 0
                                    0
                                    (round
                                     (* (texture-src-width texture)
                                        (/ width (or wrap-width width))))
                                    (round
                                     (* (texture-src-height texture)
                                        (/ height (or wrap-height height)))))))))))

(defmethod release-resources ((sprite static-sprite))
  (with-slots (sprite-draw-component) sprite
    (release-resources sprite-draw-component)
    (release-resources (draw-component sprite))))

@export
(defun add-color-map (static-sprite color-map)
  "Apply COLOR-MAP to STATIC-SPRITE. See doc for color-map struct for details."
  (declare (static-sprite static-sprite)
           (color-map color-map))
  (with-slots (color-maps) static-sprite
    (if color-maps
        (error "multiple color maps not yet supported")
        (setf color-maps (make-array 1
                                     :element-type 'color-map
                                     :initial-contents (list color-map)
                                     :adjustable t)))))

@export
(defun flip (sprite direction)
  "Toggle STATIC-SPRITE in the given DIRECTION.
A DIRECTION of :NONE will clear all flips"
  (declare (static-sprite sprite) (keyword direction))
  (with-slots (flip-list sprite-source-flip-vector) sprite
    (ecase direction
      (:none (setf flip-list (list)))
      ((:horizontal :vertical)
       (if (find direction flip-list)
           (setf flip-list (delete direction flip-list))
           (push direction flip-list))))
    (if (find :horizontal flip-list)
        (setf (elt sprite-source-flip-vector 0) -1.0)
        (setf (elt sprite-source-flip-vector 0) 1.0))
    (if (find :vertical flip-list)
        (setf (elt sprite-source-flip-vector 1) -1.0)
        (setf (elt sprite-source-flip-vector 1) 1.0))))
