(in-package :recurse.vert)

@export-class
(defstruct active-area
  "Defines an area in a game-scene (base world coords). Used to limit which objects are updated/rendered during a game loop."
  (min-x nil :type (or null single-float))
  (max-x nil :type (or null single-float))
  (min-y nil :type (or null single-float))
  (max-y nil :type (or null single-float)))
(export 'make-active-area)

@export-class
(defclass game-scene (scene)
  ((scene-background :initform nil
                     :initarg :background
                     :type scene-background
                     :accessor scene-background)
   (scene-music :initarg :music
                :initform nil
                :accessor scene-music
                :documentation "Music which will play when the scene initializes.")
   (width :initarg :width
          :initform (error ":width required")
          :reader width)
   (height :initarg :height
           :initform (error ":height required")
           :reader height)
   (update-area :initarg :update-area :initform nil)
   (render-queue :initform (make-instance 'render-queue))
   (removed-objects :initform (make-array 10
                                          :element-type '(or game-object null)
                                          :adjustable t
                                          :fill-pointer 0
                                          :initial-element nil))
   (update-queue :initform (make-array 160
                                       :element-type '(or null game-object)
                                       :initial-element nil))
   (update-queue-fill-pointer :initform 0)
   (render-rebuild-radius
    :initarg :render-rebuild-radius
    :initform #.(* 20 16)
    :documentation "Make a rect centered on camera.
Each side of the rect will be at least as big as this slot.
When camera moves outside this rect, rebuild the render queue.
This is an optimization so we don't have to rebuild the render-queue every frame.")
   (render-rebuild-camera-position
    :initform (vector2)
    :documentation "Centered camera position used to compute render-queue rebuilds.")
   (reset-instance-renderers
    :initform (make-array 5
                          :adjustable t
                          :fill-pointer 0)
    :documentation "Bookkeeping sequence of instance renderers which have been reset in the current frame.")
   (spatial-partition :initform nil
                      :documentation "Optimized spatial partition containing every object in the scene."
                      :reader spatial-partition))
  (:documentation "A scene which updates and renders game-objects."))

(defmethod initialize-instance :after ((game-scene game-scene) &rest args)
  (declare (ignore args))
  (with-slots (spatial-partition) game-scene
    (setf spatial-partition
          (make-instance 'quadtree))))

@export
(defgeneric add-to-scene (scene object)
  (:documentation "Add an object to the game scene")
  (:method ((scene scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (unless (find overlay scene-overlays)
        (vector-push-extend overlay scene-overlays)
        overlay)))
  (:method ((scene game-scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (unless (find overlay scene-overlays)
        (vector-push-extend overlay scene-overlays)
        overlay)))
  (:method ((scene game-scene) (object game-object))
    (when (start-tracking (spatial-partition scene) object)
      (add-subscriber object scene killed)
      object)))

@export
(defgeneric remove-from-scene (scene object)
  (:documentation "Remove an object from the game scene")
  (:method ((scene scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (when (find overlay scene-overlays)
        (setf scene-overlays (delete overlay scene-overlays))
        overlay)))
  (:method ((scene game-scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (when (find overlay scene-overlays)
        (setf scene-overlays (delete overlay scene-overlays))
        (render-queue-remove (slot-value scene 'render-queue) overlay)
        overlay)))
  (:method ((scene game-scene) (object game-object))
    ;; remove object at the start of the next frame to allow pending actions to finish
    (when (in-scene-p scene object)
      (remove-subscriber object scene killed)
      (stop-tracking (spatial-partition scene) object)
      (render-queue-remove (slot-value scene 'render-queue) object)
      (vector-push-extend object (slot-value scene 'removed-objects))
      object)))

(defgeneric found-object-to-update (game-scene game-object)
  (:documentation "for subclasses to hook object updates")
  (:method ((scene game-scene) game-object)))

(defun %object-was-removed-p (scene object)
  (declare (optimize (speed 3)))
  (loop :for removed :across (the (vector game-object) (slot-value scene 'removed-objects)) :do
       (when (eq object removed)
         (return t))))

(defmethod update ((game-scene game-scene) delta-t-ms (context null))
  (declare (optimize (speed 3)))
  (with-slots (update-area
               (queue render-queue)
               render-rebuild-radius
               render-rebuild-camera-position
               update-queue
               update-queue-fill-pointer
               reset-instance-renderers
               (bg scene-background)
               scene-overlays
               removed-objects
               camera)
      game-scene
    (declare ((simple-array (or null game-object)) update-queue)
             ((integer 0 100000) update-queue-fill-pointer))
    (setf (fill-pointer removed-objects) 0
          update-queue-fill-pointer 0)
    (let* ((x-min (when update-area (active-area-min-x update-area)))
           (x-max (when update-area (active-area-max-x update-area)))
           (y-min (when update-area (active-area-min-y update-area)))
           (y-max (when update-area (active-area-max-y update-area)))
           (rebuild-render-queue-p (block camera-moved-outside-render-area-p
                                     (with-accessors ((c-x x) (c-y y) (c-w width) (c-h height)) camera
                                       (declare (single-float c-x c-y c-w c-h))
                                       (let* ((camera-centered-x (+ c-x (/ c-w 2.0)))
                                              (camera-centered-y (+ c-y (/ c-h 2.0)))
                                              (delta (max
                                                      (abs (- camera-centered-x (x render-rebuild-camera-position)))
                                                      (abs (- camera-centered-y (y render-rebuild-camera-position))))))
                                         (when (>= delta render-rebuild-radius)
                                           (setf (x render-rebuild-camera-position) camera-centered-x
                                                 (y render-rebuild-camera-position) camera-centered-y)
                                           t))))))
      (declare ((or null single-float) x-min x-max y-min y-max))
      (with-slots ((bg scene-background) scene-overlays) game-scene
        (when rebuild-render-queue-p
          (setf (fill-pointer reset-instance-renderers) 0)
          (render-queue-reset queue))
        ;; pre-update frame to mark positions
        (pre-update (camera game-scene))
        (when bg
          (pre-update bg)
          (when rebuild-render-queue-p
            (render-queue-add queue bg)))
        (loop :for overlay :across (the (vector overlay) scene-overlays) :do
             (pre-update overlay))

        ;; call super
        (call-next-method game-scene delta-t-ms context)

        ;; update frame
        (let* ((render-x-min (if rebuild-render-queue-p
                                 (- (x render-rebuild-camera-position)
                                    (width camera)
                                    render-rebuild-radius)
                                 0.0))
               (render-x-max (if rebuild-render-queue-p
                                 (+ (x render-rebuild-camera-position)
                                    (width camera)
                                    render-rebuild-radius)
                                 0.0))
               (render-y-min (if rebuild-render-queue-p
                                 (- (y render-rebuild-camera-position)
                                    (height camera)
                                    render-rebuild-radius)
                                 0.0))
               (render-y-max (if rebuild-render-queue-p
                                 (+ (y render-rebuild-camera-position)
                                    (height camera)
                                    render-rebuild-radius)
                                 0.0)))
          (declare (single-float render-x-min render-x-max render-y-min render-y-max))
          (flet ((in-render-area-p (game-object)
                   (multiple-value-bind (x y z w h) (world-dimensions game-object)
                     (declare (ignore z)
                              (single-float x y w h))
                     (and (or (<= render-x-min x render-x-max)
                              (<= render-x-min (+ x w) render-x-max)
                              (and (<= x render-x-min)
                                   (>= (+ x w) render-x-max)))
                          (or (<= render-y-min y render-y-max)
                              (<= render-y-min (+ y h) render-y-max)
                              (and (<= y render-y-min)
                                   (>= (+ y h) render-y-max)))))))
            (declare (inline in-render-area-p))
            (do-spatial-partition (game-object
                                   (spatial-partition game-scene)
                                   :static-iteration-p t
                                   :min-x x-min :max-x x-max
                                   :min-y y-min :max-y y-max)
              (when (and (not (typep game-object 'static-object))
                         (not (block already-in-queue-p
                                (loop :for i :from 0 :below update-queue-fill-pointer :do
                                     (when (eq game-object (elt update-queue i))
                                       (return t))))))
                (when (= update-queue-fill-pointer (length update-queue))
                  (log:info "Doubling update queue size: ~A -> ~A"
                            update-queue-fill-pointer
                            (* 2 update-queue-fill-pointer))
                  (setf update-queue (simple-array-double-size update-queue)))
                (setf (elt update-queue update-queue-fill-pointer) game-object)
                (incf update-queue-fill-pointer)
                (pre-update game-object))
              (when (and rebuild-render-queue-p
                         (in-render-area-p game-object)
                         (not (%object-was-removed-p game-scene game-object)))
                (when (typep game-object 'instance-rendered-drawable)
                  (with-slots ((instance-renderer instance-renderer)) game-object
                    (unless (find instance-renderer reset-instance-renderers)
                      (vector-push-extend instance-renderer reset-instance-renderers)
                      (instance-renderer-reset instance-renderer))))
                (render-queue-add queue game-object)))))
        (update (camera game-scene) delta-t-ms game-scene)
        (loop :for overlay :across (the (vector overlay) scene-overlays) :do
             (update overlay delta-t-ms game-scene)
             (when rebuild-render-queue-p
               (render-queue-add queue overlay)))
        (when rebuild-render-queue-p
          (render-queue-add queue camera))
        (when bg
          (update bg delta-t-ms game-scene))
        (loop :for i :from 0 :below update-queue-fill-pointer :do
             (let ((game-object (elt update-queue i)))
               (found-object-to-update game-scene game-object)
               (update game-object delta-t-ms game-scene)))
        (values)))))

(defmethod render ((game-scene game-scene) update-percent (camera simple-camera) renderer)
  (declare (optimize (speed 3)))
  (with-slots ((bg scene-background)
               spatial-partition
               (queue render-queue)
               scene-overlays)
      game-scene
    (render queue update-percent camera renderer))
  (values))

(defevent-callback killed ((object obb) (game-scene game-scene))
  (remove-from-scene game-scene object))

;; TODO: remove this fn and use scheduler util directly
@export
(defun schedule (game-scene timestamp zero-arg-fn)
  "When the value returned by SCENE-TICKS of GAME-SCENE equals or exceeds TIMESTAMP the ZERO-ARG-FN callback will be invoked."
  (scheduler-add game-scene timestamp zero-arg-fn)
  (values))

@export
(defun get-object-by-id (scene id)
  "Return the (presumably) unique game-object identified by ID in SCENE."
  (declare (game-scene scene))
  (block find-object
    (do-spatial-partition (game-object (spatial-partition scene) :static-iteration-p t)
      (when (equalp (object-id game-object) id)
        (return-from find-object game-object)))))

@export
(defun in-scene-p (scene object)
  "Return OBJECT if OBJECT is in SCENE, nil otherwise."
  (declare (optimize (speed 3))
           (game-scene scene))
  (block find-object
    (do-spatial-partition (obj (spatial-partition scene) :static-iteration-p t)
      (when (eq obj object)
        (return-from find-object object)))))
