(in-package :recurse.vert)

(defclass %sprite-quad (gl-quad)
  ((texture-id :initform nil)))

(defmethod render ((quad %sprite-quad) update-percent camera gl-context)
  (with-slots (texture-id) quad
    (when texture-id
      (setf (gl-drawable-input-texture quad)
            texture-id)

      (with-slots (texture-src) quad
        #+nil
        (setf
         (elt texture-src 0) 0.0
         (elt texture-src 1) 0.0
         (elt texture-src 2) 1.0
         (elt texture-src 3) -1.0)
        (setf
         (elt texture-src 0) 0.0

         (elt texture-src 1) 0.0

         (elt texture-src 2)
         (/ 1.0 7.0)

         (elt texture-src 3)
         ;; (/ 1.0 4.0)
         (- (/ 1.0 4.0))
         ))))
  (call-next-method quad update-percent camera gl-context))

@export-class
(defclass sprite (gl-pipeline obb)
  ((path :initarg :path
         :initform (error ":path required")
         :accessor sprite-path
         :documentation "Location of the sprite png to render. If null only the color mod will render.")
   (color :initarg :color
          :initform nil
          :accessor color
          :documentation "Optional color mod to apply to the sprite.")
   (texture :initform nil
            :documentation "opengl texture wrapper")
   (quad :initform nil)
   (sprite-releaser :initform nil))
  (:documentation "A GL-PIPELINE which renders a sprite in the first pipeline stage."))

(defmethod initialize-instance :around ((sprite sprite) &rest args)
  ;; NOTE: consing
  (let ((all-args (append (list sprite) args)))
    (prog1 (apply #'call-next-method all-args)
      (with-slots (path quad color) sprite
        (setf quad (make-instance '%sprite-quad
                                  :color color
                                  :render-area sprite))
        (gl-pipeline-add sprite quad))
      (with-slots (texture path) sprite
        (setf texture (make-instance 'sprite-backed-texture
                                     :path path)))
      (resource-autoloader-add-object *resource-autoloader*
                                      (tg:make-weak-pointer sprite)))))

(defmethod (setf color) :after (new-color (sprite sprite))
  (with-slots (quad color) sprite
    (setf (color quad) color)))

;; TODO
;; (defmethod (setf sprite-path) :around (new-sprite-path (static-sprite static-sprite))
;;   (let ((old-path (path-to-sprite static-sprite)))
;;     (prog1 (call-next-method new-sprite-path static-sprite)
;;       (unless (or (equal old-path (path-to-sprite static-sprite))
;;                   (null *engine-manager*))
;;         (release-resources static-sprite)
;;         (load-resources static-sprite)))))

(defmethod load-resources ((sprite sprite))
  (prog1 (call-next-method sprite)
    (unless (slot-value sprite 'sprite-releaser)
      (with-accessors ((texture-id gl-drawable-input-texture)) sprite
        (with-slots (texture quad) sprite
          (load-resources texture)
          (setf (slot-value quad 'texture-id)
                (texture-id texture)))
        (let ((texture (slot-value sprite 'texture)))
          (setf (slot-value sprite 'sprite-releaser)
                (make-resource-releaser (sprite)
                  (%release-sprite-resources texture))))))))

(defun %release-sprite-resources (texture)
  (release-resources texture))

(defmethod release-resources ((sprite sprite))
  (prog1 (call-next-method sprite)
    (with-slots (sprite-releaser texture quad) sprite
      (when sprite-releaser
        (%release-sprite-resources texture)
        (cancel-resource-releaser sprite-releaser)
        (setf (slot-value quad 'texture-id) nil
              sprite-releaser nil)))))
