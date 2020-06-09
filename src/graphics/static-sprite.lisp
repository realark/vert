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
            texture-id)))
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
   (flip-list :initform (make-array 0
                                    :fill-pointer 0
                                    :adjustable t
                                    :element-type 'keyword)))
  (:documentation "A GL-PIPELINE which renders a sprite in the first pipeline stage."))

(defmethod initialize-instance :around ((sprite static-sprite) &rest args)
  ;; NOTE: consing
  (let ((all-args (append (list sprite) args)))
    (prog1 (apply #'call-next-method all-args)
      (with-slots ((path path-to-sprite) quad color) sprite
        (setf quad (make-instance '%sprite-quad
                                  :render-priority -1
                                  :color color))
        (gl-pipeline-add sprite quad))
      (with-slots (texture (path path-to-sprite)) sprite
        (setf texture (make-instance 'sprite-backed-texture
                                     :path path)))
      (with-slots (wrap-width wrap-height sprite-source) sprite
        (when (and (or wrap-width wrap-height)
                   sprite-source)
          (error "using wrap-width/height with sprite-source not supported. ~A" sprite)))

      (resource-autoloader-add-object *resource-autoloader*
                                      (tg:make-weak-pointer sprite)))))

(defmethod load-resources ((sprite static-sprite))
  (prog1 (call-next-method sprite)
    (unless (slot-value sprite 'sprite-releaser)
      (with-accessors ((texture-id gl-drawable-input-texture)) sprite
        (with-slots (texture quad wrap-width wrap-height sprite-source) sprite
          (load-resources texture)
          (setf (slot-value quad 'texture-id)
                (texture-id texture))

          (when (or wrap-width wrap-height)
            (with-accessors ((width width) (height height)) sprite
              (setf sprite-source
                    (make-sprite-source 0
                                        0
                                        (round
                                         (* (texture-src-width texture)
                                            (/ width (or wrap-width width))))
                                        (round
                                         (* (texture-src-height texture)
                                            (/ height (or wrap-height height)))))))))
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

(defmethod render ((sprite static-sprite) update-percent camera gl-context)
  (with-slots (quad) sprite
    ;; TODO stop hardcoding texture-src
    (block set-sprite-source
      (with-slots (texture sprite-source sprite-source-flip-vector) sprite
        (with-slots (texture-src) quad
          (let* ((source (or sprite-source *default-sprite-source*))
                 (flip-x (elt sprite-source-flip-vector 0))
                 (flip-y (elt sprite-source-flip-vector 1))
                 (x (sprite-source-x source))
                 (y (sprite-source-y source))
                 (total-w (texture-src-width texture))
                 (total-h (texture-src-height texture))
                 (w (or (sprite-source-w source) total-w))
                 (h (or (sprite-source-h source) total-h)))
            (declare ((single-float -1.0 1.0) flip-x flip-y)
                     ((integer 0 *) x y w h total-w total-h))
            (setf
             ;; x
             (elt texture-src 0)
             (if (< flip-x 0)
                 (/ (float (+ x w)) total-w)
                 (/ (float x) total-w))
             ;; y
             (elt texture-src 1)
             (if (< flip-y 0.0)
                 (- 1.0 (/ (float (+ y h)) total-h))
                 ;; invert y coord for upper-left coord
                 (- 1.0 (/ (float y) total-h)))
             ;; w
             (elt texture-src 2)
             (float (/ (* w flip-x) total-w))
             ;; h
             (elt texture-src 3)
             ;; invert y coord for upper-left coord
             (if (< flip-y 0.0)
                 (+ (float (/ h total-h)))
                 (- (float (/ h total-h))))))))))
  (call-next-method sprite update-percent camera gl-context))

(defmethod (setf color) :after (new-color (sprite static-sprite))
  (with-slots (quad color) sprite
    (when quad
      (setf (color quad) color))))

(defmethod (setf path-to-sprite) :around (new-path (sprite static-sprite))
  (let ((old-path (path-to-sprite sprite)))
    (prog1 (call-next-method new-path sprite)
      (unless (or (equal old-path (path-to-sprite sprite))
                  (null (slot-value sprite 'sprite-releaser))
                  (null *engine-manager*))
        (release-resources sprite)
        (load-resources sprite)))))

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
(defmethod flip ((sprite static-sprite) direction)
  "Toggle STATIC-SPRITE in the given DIRECTION.
A DIRECTION of :NONE will clear all flips"
  (declare (keyword direction))
  (with-slots (flip-list sprite-source-flip-vector) sprite
    (ecase direction
      (:none (setf (fill-pointer flip-list) 0))
      ((:horizontal :vertical)
       (if (find direction flip-list)
           (setf flip-list (delete direction flip-list))
           (vector-push-extend direction flip-list))))
    (if (find :horizontal flip-list)
        (setf (elt sprite-source-flip-vector 0) -1.0)
        (setf (elt sprite-source-flip-vector 0) 1.0))
    (if (find :vertical flip-list)
        (setf (elt sprite-source-flip-vector 1) -1.0)
        (setf (elt sprite-source-flip-vector 1) 1.0))))
