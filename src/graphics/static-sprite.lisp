(in-package :recurse.vert)

;;;; sprite structs and helpers

@export-structure
(defstruct (sprite-source (:constructor %make-sprite-source))
  "Source rectangle for a sprite. w and h may be nil to specify 100%."
  (x 0 :type (integer 0 *))
  (y 0 :type (integer 0 *))
  (w nil :type (or null (integer 1 *)))
  (h nil :type (or null (integer 1 *))))

@export
(defun make-sprite-source (x y w h)
  (%make-sprite-source :x x :y y :w w :h h))

(defvar *default-sprite-source*
  (make-sprite-source 0 0 nil nil)
  "A sprite-source which shows the entire sprite")

@export-structure
(defstruct color-map
  "A COLOR-MAP can be applied to a sprite to convert all matching FROM-COLORs to TO-COLOR in the sprite within the TOLERANCE range."
  (from-color *white* :type color)
  (to-color *white* :type color)
  (tolerance (/ 3.0 255.0) :type (single-float 0.0 1.0)))

(defvar %no-op-color-map%
  (make-color-map :from-color *white*
                  :to-color *white*
                  :tolerance 0.0))

(defvar *sprite-buffer-cache*
  (getcache-default "sprite-buffer-cache"
                    *engine-caches*
                    (make-instance 'counting-cache
                                   :on-evict
                                   (lambda (sprite-key gl-buffers)
                                     (declare (ignore sprite-key))
                                     (destructuring-bind (cached-vao cached-vbo) gl-buffers
                                       (gl:delete-vertex-arrays (list cached-vao))
                                       (gl:delete-buffers (list cached-vbo))))))
  "Cache of gl-sprie's VAO and VBO")

(defclass %sprite-quad (gl-quad)
  ((texture-id :initform nil)))

(defmethod render ((quad %sprite-quad) update-percent camera gl-context)
  (with-slots (texture-id) quad
    (when texture-id
      (setf (gl-drawable-input-texture quad)
            texture-id)

      ;; TODO stop hardcoding texture-src
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

;;;; sprite class

@export-class
(defclass static-sprite (gl-pipeline obb)
  ((sprite-releaser :initform nil)
   (path-to-sprite :initarg :path-to-sprite
                   :initform nil
                   :accessor path-to-sprite
                   :documentation "Filesystem location of sprite. May be null to only render the color mod.")
   (color :initarg :color
          :initform nil
          :accessor color
          :documentation "Optional color mod to apply to the sprite.")
   (texture :initform nil
            :documentation "opengl texture wrapper")
   (quad :initform nil)
   (color-maps :initform nil
               :documentation "Before color mod is applied, allow mapping src colors to dest colors")
   (sprite-source :initarg :sprite-source
                  :initform nil
                  :accessor sprite-source
                  :documentation "Rectangle subset of the sprite to render.
Nil to render the entire sprite.")
   (sprite-source-flip-vector :initform (vector2 1.0 1.0)
                              :reader sprite-source-flip-vector
                              :documentation "Flip the vertical or horizontal axis of the sprite.")
   (wrap-width :initform nil
               :initarg :wrap-width
               :documentation "repeat the sprite texture horizontally after wrap-width is exceeded")
   (wrap-height :initform nil
                :initarg :wrap-height
                :documentation "repeat the sprite texture vertically after wrap-height is exceeded")
   ;; TODO: consting vv
   (flip-list :initform (list)
              :accessor flip-list))
  (:documentation "A GL-PIPELINE which renders a sprite in the first pipeline stage."))

(defmethod initialize-instance :around ((sprite static-sprite) &rest args)
  ;; NOTE: consing
  (let ((all-args (append (list sprite) args)))
    (prog1 (apply #'call-next-method all-args)
      (with-slots ((path path-to-sprite) quad color) sprite
        (setf quad (make-instance '%sprite-quad
                                  :color color))
        (gl-pipeline-add sprite quad))
      (with-slots (texture (path path-to-sprite)) sprite
        (setf texture (make-instance 'sprite-backed-texture
                                     :path path)))
      ;; setting the render-area of this sprite's pipeline to itself.
      ;; A little weird, but this allows changes to the sprite's position/dimensions
      ;; to automatically be reflected in the render area.
      (setf (gl-pipeline-render-area sprite) sprite)

      (resource-autoloader-add-object *resource-autoloader*
                                      (tg:make-weak-pointer sprite)))))

(defmethod load-resources ((sprite static-sprite))
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

(defmethod release-resources ((sprite static-sprite))
  (prog1 (call-next-method sprite)
    (with-slots (sprite-releaser texture quad) sprite
      (when sprite-releaser
        (%release-sprite-resources texture)
        (cancel-resource-releaser sprite-releaser)
        (setf (slot-value quad 'texture-id) nil
              sprite-releaser nil)))))

(defmethod (setf color) :after (new-color (sprite static-sprite))
  (with-slots (quad color) sprite
    (when quad
      (setf (color quad) color))))

;; TODO
#+nil
(defmethod (setf path-to-sprite) :around (new-sprite-path (static-sprite static-sprite))
  (let ((old-path (path-to-sprite static-sprite)))
    (prog1 (call-next-method new-sprite-path static-sprite)
      (unless (or (equal old-path (path-to-sprite static-sprite))
                  (null *engine-manager*))
        (release-resources static-sprite)
        (load-resources static-sprite)))))

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
(defun get-color-maps (static-sprite)
  (slot-value static-sprite 'color-maps))


@export
(defmethod flip (sprite direction)
  "Toggle STATIC-SPRITE in the given DIRECTION.
A DIRECTION of :NONE will clear all flips"
  ;; TODO
  ;; (declare (static-sprite sprite) (keyword direction))
  #+nil
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
