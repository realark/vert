(in-package :recurse.vert)

@export-class
(defclass game-scene (scene gl-pipeline)
  ((scene-background :initform nil
                     :initarg :background
                     :type scene-background
                     :accessor scene-background)
   (scene-audio-state :initform nil
                      :documentation "Used to resume audio-state when the scene deactivates.")
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
   (live-object-radius
    :initarg :live-object-radius
    :initform #.(* 15 16)
    :documentation "Make a rect centered on camera.
The value of this slot will be the distance between the live area and camera rect.
When camera moves outside this rect, rebuild objects to render and update
This is an optimization so we don't have to rebuild the render and update queues every frame.")
   (render-queue :initform (make-instance 'render-queue
                                          :render-priority -1))
   (updatable-objects :initform (make-array 100
                                            :adjustable t
                                            :fill-pointer 0
                                            :element-type '(or null game-object)
                                            :initial-element nil))
   (updating-p :initform nil :reader updating-p)
   (pending-adds :initform (make-array 10
                                       :adjustable t
                                       :fill-pointer 0
                                       :element-type '(or null game-object)
                                       :initial-element nil)
                 :documentation "Objects to be added to scene at the start of the next frame.")
   (pending-removes :initform (make-array 10
                                          :adjustable t
                                          :fill-pointer 0
                                          :element-type '(or null game-object)
                                          :initial-element nil)
                    :documentation "Objects to be removed from scene at the start of the next frame.")
   (live-object-rebuild-camera-position
    :initform (vector2)
    :documentation "Centered camera position used to compute render-queue rebuilds.")
   (reset-instance-renderers
    :initform (make-array 5
                          :adjustable t
                          :fill-pointer 0)
    :documentation "Sequence of instance renderers which have been reset in the current frame.")
   (spatial-partition :initform nil
                      :documentation "Optimized spatial partition containing every object in the scene."
                      :reader spatial-partition))
  (:documentation "A scene which updates and renders game-objects."))

(defmethod initialize-instance :after ((game-scene game-scene) &rest args)
  (declare (ignore args))
  (with-slots (spatial-partition render-queue) game-scene
    (gl-pipeline-add game-scene render-queue)
    (setf spatial-partition
          (make-instance 'quadtree))))

@export
(defgeneric add-to-scene (scene object)
  (:documentation "Add an object to the game scene")
  (:method ((scene scene) (overlay overlay))
    (with-slots (scene-overlays render-queue) scene
      (unless (find overlay scene-overlays)
        (vector-push-extend overlay scene-overlays)
        overlay)))
  (:method ((scene game-scene) (overlay overlay))
    (with-slots (scene-overlays render-queue) scene
      (unless (find overlay scene-overlays)
        (vector-push-extend overlay scene-overlays)
        (render-queue-add render-queue overlay)
        overlay)))
  (:method ((scene game-scene) (object game-object))
    (if (updating-p scene)
        (with-slots (pending-adds pending-removes) scene
          (if (in-scene-p scene object)
              (when (find object pending-removes)
                (log:debug "cancel ~A for scene remove" object)
                (setf pending-removes (delete object pending-removes))
                object)
              (unless (find object pending-adds)
                (log:debug "queuing ~A for scene add" object)
                (vector-push-extend object pending-adds)
                object)))
        ;; fast path for adding objects outside of scene update (i.e. initialization)
        (%%add-object-to-scene scene object))))

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
    (with-slots (pending-adds pending-removes) scene
      (if (in-scene-p scene object)
          (unless (find object pending-removes)
            (log:debug "queuing ~A for scene removal" object)
            (vector-push-extend object pending-removes)
            (unless (updating-p scene)
              (%run-pending-removes scene))
            object)
          (when (find object pending-adds)
            (log:debug "cancel ~A for scene add" object)
            (setf pending-adds (delete object pending-adds))
            object)))))

(defmethod scene-activated ((scene game-scene))
  (with-slots ((state scene-audio-state)) scene
    (if state
        (audio-player-load-state *audio* state)
        (audio-player-stop-all *audio*))))

(defmethod scene-deactivated ((scene game-scene))
  (with-slots ((state scene-audio-state)) scene
    (with-sdl-mixer-lock-held
      (unless state
        (setf state (audio-player-copy-state *audio*)))
      (audio-player-copy-state *audio* state)
      (audio-player-stop-music *audio*)
      (audio-player-stop-sfx *audio*)))
  (values))

@export
(defun scene-teleport-object (scene object &optional new-x new-y new-z)
  "Move OBJECT within SCENE to the new coordinates instantly. OBJECT's position will be recycled internally so it will instantly appear in the new position with no position interpolation."
  (when new-x
    (setf (x object) new-x))
  (when new-y
    (setf (y object) new-y))
  (when new-z
    (setf (z object) new-z))
  (recycle object)
  (when (%in-live-object-area-p scene object)
    (with-slots (render-queue updatable-objects) scene
      (render-queue-add render-queue object)
      (unless (find object updatable-objects :test #'eq)
        (vector-push-extend object updatable-objects))))
  object)

(defgeneric found-object-to-update (game-scene game-object)
  (:documentation "for subclasses to hook object updates")
  (:method ((scene game-scene) game-object)))

(defun %%add-object-to-scene (scene object)
  (declare (optimize (speed 3))
           (game-scene scene)
           (game-object object))
  (with-slots (spatial-partition render-queue updatable-objects) scene
    (when (start-tracking spatial-partition object)
      (add-subscriber object scene killed)
      (when (%in-live-object-area-p scene object)
        (render-queue-add render-queue object)
        (unless (find object updatable-objects :test #'eq)
          (vector-push-extend object updatable-objects)))
      object)))

(defun %run-pending-removes (scene)
  (declare (optimize (speed 3))
           (game-scene scene))
  (with-slots (pending-removes spatial-partition render-queue updatable-objects) scene
    (declare (vector pending-removes updatable-objects))
    (when (> (length pending-removes) 0)
      (loop :for removed-object :across pending-removes :do
           (remove-subscriber removed-object scene killed)
           (stop-tracking spatial-partition removed-object)
           (when (%in-live-object-area-p scene removed-object)
             (render-queue-remove render-queue removed-object)
             (setf updatable-objects (delete removed-object updatable-objects)))
           (log:debug "removed ~A from scene" removed-object)
         :finally
           (setf (fill-pointer pending-removes) 0))))
  (values))

(defun %run-pending-adds (scene)
  (declare (optimize (speed 3))
           (game-scene scene))
  (with-slots (pending-adds spatial-partition render-queue updatable-objects) scene
    (loop :for object :across pending-adds :do
         (%%add-object-to-scene scene object)
       :finally
         (setf (fill-pointer pending-adds) 0))))

(defun %force-rebuild-live-objects (scene)
  (log:debug "force live object rebuild.")
  (with-slots (camera live-object-radius live-object-rebuild-camera-position) scene
    (if (float= 0.0 (x live-object-rebuild-camera-position))
        (setf (x live-object-rebuild-camera-position)
              (+ (width camera) live-object-radius))
        (setf (x live-object-rebuild-camera-position) 0.0))))

(defun %in-live-object-area-p (scene game-object)
  "T if OBJECT is inside SCENE's current live object area."
  (declare (optimize (speed 3))
           (game-scene scene)
           (game-object game-object))
  (with-slots (camera live-object-radius live-object-rebuild-camera-position) scene
    (let ((live-x-min (- (x live-object-rebuild-camera-position)
                         (width camera)
                         live-object-radius))
          (live-x-max (+ (x live-object-rebuild-camera-position)
                         (width camera)
                         live-object-radius))
          (live-y-min (- (y live-object-rebuild-camera-position)
                         (height camera)
                         live-object-radius))
          (live-y-max (+ (y live-object-rebuild-camera-position)
                         (height camera)
                         live-object-radius)))
      (multiple-value-bind (x y z w h) (world-dimensions game-object)
        (declare (ignore z)
                 (single-float x y w h))
        (and (or (<= live-x-min x live-x-max)
                 (<= live-x-min (+ x w) live-x-max)
                 (and (<= x live-x-min)
                      (>= (+ x w) live-x-max)))
             (or (<= live-y-min y live-y-max)
                 (<= live-y-min (+ y h) live-y-max)
                 (and (<= y live-y-min)
                      (>= (+ y h) live-y-max))))))))

(defun %rebuild-live-object-area-p (scene)
  (declare (optimize (speed 3))
           (game-scene scene))
  (block camera-moved-outside-render-area-p
    (with-slots (camera live-object-radius live-object-rebuild-camera-position) scene
      (with-accessors ((c-x x) (c-y y) (c-w width) (c-h height)) camera
        (declare (single-float c-x c-y c-w c-h))
        (let* ((camera-centered-x (+ c-x (/ c-w 2.0)))
               (camera-centered-y (+ c-y (/ c-h 2.0)))
               (delta (max
                       (abs (- camera-centered-x (x live-object-rebuild-camera-position)))
                       (abs (- camera-centered-y (y live-object-rebuild-camera-position))))))
          (when (>= delta live-object-radius)
            (setf (x live-object-rebuild-camera-position) camera-centered-x
                  (y live-object-rebuild-camera-position) camera-centered-y)
            t))))))

(defmethod update :around ((scene game-scene))
  (with-slots (updating-p) scene
    (setf updating-p t)
    (unwind-protect
         (call-next-method scene)
      (setf updating-p nil))))

(defmethod update ((game-scene game-scene))
  (declare (optimize (speed 3)))
  (with-slots (live-object-rebuild-camera-position
               live-object-radius
               updatable-objects
               (queue render-queue)
               reset-instance-renderers
               (bg scene-background)
               scene-overlays
               pending-removes
               camera)
      game-scene
    (let ((rebuild-live-objects-p (%rebuild-live-object-area-p game-scene)))
      (%run-pending-removes game-scene)
      (%run-pending-adds game-scene)
      (when rebuild-live-objects-p
        (setf (fill-pointer updatable-objects) 0)
        (render-queue-reset queue)
        (setf (fill-pointer reset-instance-renderers) 0))
      ;; pre-update frame to mark positions
      (pre-update (camera game-scene))
      (when bg
        (pre-update bg)
        (when rebuild-live-objects-p
          (render-queue-add queue bg)))
      (loop :for overlay :across (the (vector overlay) scene-overlays) :do
           (pre-update overlay))

      ;; call super
      (call-next-method game-scene)

      ;; update frame
      (when rebuild-live-objects-p
        (let ((num-objects-to-update 0)
              (num-objects-to-render 0)
              (live-x-min (- (x live-object-rebuild-camera-position)
                             (width camera)
                             live-object-radius))
              (live-x-max (+ (x live-object-rebuild-camera-position)
                             (width camera)
                             live-object-radius))
              (live-y-min (- (y live-object-rebuild-camera-position)
                             (height camera)
                             live-object-radius))
              (live-y-max (+ (y live-object-rebuild-camera-position)
                             (height camera)
                             live-object-radius)))
          (declare (fixnum num-objects-to-render num-objects-to-update)
                   (single-float live-x-min live-x-max live-y-min live-y-max))
          (log:debug "rebuilding live-objects")
          (do-spatial-partition (game-object
                                 (spatial-partition game-scene)
                                 :static-iteration-p t
                                 :min-x live-x-min :max-x live-x-max
                                 :min-y live-y-min :max-y live-y-max)
            (block found-object-to-render
              ;; TODO: counter is slightly inaccurate because spatial partitions may visit the same object twice
              ;; to fix this, the render queue should return different values if obj is already queued
              (block check-if-instance-rendered
                (if (typep game-object 'instance-rendered-drawable)
                  (with-slots ((instance-renderer instance-renderer)) game-object
                    (unless (find instance-renderer reset-instance-renderers)
                      (incf num-objects-to-render)
                      (vector-push-extend instance-renderer reset-instance-renderers)
                      (instance-renderer-reset instance-renderer game-scene)))
                  (incf num-objects-to-render)))
              (render-queue-add queue game-object))
            (block check-add-to-updatable-objects
              (when (and (not (typep game-object 'static-object))
                         (not (find game-object updatable-objects :test #'eq)))
                (incf num-objects-to-update)
                (vector-push-extend game-object updatable-objects))))
          (log:debug "Rebuild complete. Found ~A objects to render and ~A objects to update"
                     num-objects-to-render
                     num-objects-to-update)))
      (update (camera game-scene))
      (loop :for overlay :across (the (vector overlay) scene-overlays) :do
           (update overlay)
           (when rebuild-live-objects-p
             (render-queue-add queue overlay)))
      (when rebuild-live-objects-p
        (render-queue-add queue camera))
      (when bg
        (update bg))
      (loop :for game-object :across updatable-objects :do
           (pre-update game-object)
           (found-object-to-update game-scene game-object)
           (update game-object))
      (values))))

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
