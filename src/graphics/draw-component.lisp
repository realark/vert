(in-package :recurse.vert)

;;;; Base Draw component class

@export-class
(defclass draw-component ()
  ()
  (:documentation "Component which draws a game-object"))

(defclass no-op-draw-component (draw-component)
  ())

(defmethod render ((no-op no-op-draw-component) update-percent camera context))

;;;; GL Polygon Drawer

(defclass polygon-draw (draw-component)
  ((shader :initform (getcache-default 'polygon-shader
                                       *shader-cache*
                                       (make-instance 'shader
                                                      :vertex-source
                                                      (get-builtin-shader-source 'polygon-shader.vert)
                                                      :fragment-source
                                                      (get-builtin-shader-source 'polygon-shader.frag))))
   (vao :initform 0)
   (vbo :initform 0))
  (:documentation "A draw component which renders a solid color polygon."))

(defmethod initialize-instance :after ((polygon-draw polygon-draw) &rest args)
  (declare (ignore args))
  (let ((shader (slot-value polygon-draw 'shader))
        (vao (slot-value polygon-draw 'vao))
        (vbo (slot-value polygon-draw 'vbo))
        (weak-pointer (tg:make-weak-pointer polygon-draw)))
    (resource-autoloader-add-object *resource-autoloader* weak-pointer)
    ;; note: using LET instead of WITH-SLOTS to avoid reference to polygon-draw in finalizer
    (tg:finalize polygon-draw
                 (lambda ()
                   (%release-polygon-draw-resources shader vao vbo)))))

(defmethod load-resources ((polygon-draw polygon-draw))
  (with-slots (shader vao vbo) polygon-draw
    (when (= 0 vao)
      (load-resources shader)
      (let ((buffers (%create-polygon-vao)))
        (setf vao (first buffers))
        (setf vbo (second buffers))))))

(defmethod release-resources ((polygon-draw polygon-draw))
    (with-slots (shader vao vbo) polygon-draw
      (%release-polygon-draw-resources shader vao vbo)
      (setf vao 0
            vbo 0)))

(defun %release-polygon-draw-resources (shader vao vbo)
  (unless (= 0 vao)
    (release-resources shader)
    (gl:delete-vertex-arrays (list vao))
    (gl:delete-buffers (list vbo))))

(defvar %polygon-draw% (make-instance 'polygon-draw))

(defun %render-polygon (game-object color update-percent camera renderer)
  "Render GAME-OBJECT using the global polygon shader"
  (declare (optimize (speed 3)))
  ;; - bind the geometry shader,
  ;; - set color and position
  (with-slots (shader vao) %polygon-draw%
    (gl-use-shader renderer shader)

    (if color
        (set-uniformf shader
                      "color"
                      (r color)
                      (g color)
                      (b color)
                      (a color))
        (set-uniformf shader
                      "color"
                      1.0 1.0 1.0 1.0))

    ;; set position, rotation, and size
    (let* ((sprite-transform (sprite-transform game-object)))
      (declare (dynamic-extent sprite-transform))
      (set-uniform-matrix-4fv shader
                              "worldModel"
                              sprite-transform
                              nil))

    ;; set world projection
    (set-uniform-matrix-4fv shader
                            "worldProjection"
                            (interpolated-world-projection-matrix camera update-percent)
                            nil)

    (gl-use-vao renderer vao)
    ;; TODO draw for arbitrary polygons (only rectangles supported currently)
    (n-draw-arrays :triangle-fan 0 4)
    (values)))

(defun %create-polygon-vao ()
  (let ((vao 0)
        (vbo 0)
        (gl-vertices (alloc-gl-array  :float
                                      ;; positions
                                      ;; positions
                                      0.0   0.0  0.0
                                      0.0  -1.0  0.0
                                     -1.0  -1.0  0.0
                                     -1.0   0.0  0.0
                                     )))
    (unwind-protect
         (progn
           (setf vao (gl:gen-vertex-array)
                 vbo (gl:gen-buffer))

           (gl-use-vao *gl-context* vao)

           ;; put the vertices in the VBO
           (gl:bind-buffer :array-buffer vbo)
           (gl:buffer-data :array-buffer :static-draw gl-vertices)
           ;; position
           (gl:vertex-attrib-pointer 0 3 :float 0 (* 3 (cffi:foreign-type-size :float)) (* 0 (cffi:foreign-type-size :float)))
           (gl:enable-vertex-attrib-array 0)

           ;; note: this is okay because vertex-attrib-pointer binds the vertex shader's input
           (gl:bind-buffer :array-buffer 0)
           (list vao vbo))
      (gl:free-gl-array gl-vertices))))

;;;; Dev option to draw GL primitives for hitboxes

(defvar %hitbox-render-color% (make-color-rgba :r 255 :a 100))
(defvar %phantom-render-color% (make-color-rgba :b 255 :a 100))

(defmethod render :after ((game-object game-object) update-percent camera (renderer gl-context))
  (when (get-dev-config 'dev-mode-render-collision-hitboxes)
    (cond ((typep game-object 'phantom)
           (%render-polygon game-object %phantom-render-color% 1.0 camera renderer))
          ((not (eq game-object (hit-box game-object t)))
           (%render-polygon (hit-box game-object t) %hitbox-render-color% 1.0 camera renderer)))))
