(in-package :recurse.vert)

(progn
  (defclass static-sprite (gl-sprite aabb)
    ((path-to-sprite :initarg :path-to-sprite
                     :initform (error ":path-to-sprite must be specified")
                     :accessor path-to-sprite
                     :documentation "Path to image file.")
     (wrap-width :initform nil
                 :initarg :wrap-width)
     (wrap-height :initform nil
                  :initarg :wrap-height)
     (flip-list :initform (list)
                :accessor flip-list))
    (:documentation "A game-object which loads pixels from an image resource for rendering."))

  (export '(sprite-source-x sprite-source-y sprite-source-width sprite-source-height)))


(defmethod initialize-instance :after ((sprite static-sprite) &rest args)
  (declare (ignore args))
  (with-slots (sprite-source wrap-width wrap-height) sprite
    (when (and (or wrap-width wrap-height)
               sprite-source)
      (error "using wrap-width/height with sprite-source not supported. ~A" sprite))))

(defmethod load-resources :after ((sprite static-sprite) rendering-context)
  (with-slots (wrap-width wrap-height) sprite
    (with-accessors ((sprite-source sprite-source)
                     (texture texture)
                     (width width) (height height))
        sprite
      (when (or wrap-width wrap-height)
        (setf sprite-source
              (make-sprite-source 0
                                  0
                                  (round
                                   (* (texture-src-width texture)
                                      (/ width (or wrap-width width))))
                                  (round
                                   (* (texture-src-height texture)
                                      (/ height (or wrap-height height))))))))))

@export
(defun flip (sprite direction)
  "Toggle STATIC-SPRITE in the given DIRECTION.
A DIRECTION of :NONE will clear all flips"
  (declare (static-sprite sprite) (keyword direction))
  (with-slots (flip-list flip-vector) sprite
    (ecase direction
      (:none (setf flip-list (list)))
      ((:horizontal :vertical)
       (if (find direction flip-list)
           (setf flip-list (delete direction flip-list))
           (push direction flip-list))))
    (if (find :horizontal flip-list)
        (setf (elt flip-vector 0) -1.0)
        (setf (elt flip-vector 0) 1.0))
    (if (find :vertical flip-list)
        (setf (elt flip-vector 1) -1.0)
        (setf (elt flip-vector 1) 1.0))))
