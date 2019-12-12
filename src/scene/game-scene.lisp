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
   (scene-ticks :initform 0
                :reader scene-ticks
                :documentation "Amount of milliseconds passed in the game scene.
Will be incremented by the update timestep after every update frame.")
   (scheduled-tasks :initform (make-array 0 :fill-pointer 0 :adjustable T)
                    :documentation "key-value plist-vector of (timestamp zero-arg-fn). When SCENE-TICKS equal or exceed the timestamp, the lambda will be invoked.
List is ascending timestamp ordered.")
   (scene-music :initarg :music
                :initform nil
                :accessor scene-music
                :documentation "Music which will play when the scene initializes.")
   (unloaded-game-objects :initform (list)
                          :accessor unloaded-game-objects
                          :documentation "Game-Objects which have been added but not yet loaded.
On the next render frame, the objects will be given a chance to load and this list will be emptied.")
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
   (spatial-partition :initform nil
                      :reader spatial-partition))
  (:documentation "A game world."))

(defmethod initialize-instance :after ((game-scene game-scene) &rest args)
  (declare (ignore args))
  (with-slots (spatial-partition) game-scene
    (setf spatial-partition
          (make-instance 'quadtree))))

(defmethod load-resources ((game-scene game-scene) renderer)
  (with-accessors ((music scene-music)
                   (bg scene-background))
      game-scene
    (when bg
      (load-resources bg renderer))
    (do-spatial-partition (game-object (spatial-partition game-scene) :static-iteration-p t)
      (load-resources game-object renderer))
    (loop :for overlay :across (the (vector overlay) (slot-value game-scene 'scene-overlays)) :do
         (load-resources overlay renderer))
    (when music
      ;; Hack to resume music on unpause
      (if (eq :paused (music-state *audio*))
          (setf (music-state *audio*) :playing)
          (play-music *audio* music :num-plays -1)))))

(defmethod release-resources ((game-scene game-scene))
  (do-spatial-partition (game-object (spatial-partition game-scene) :static-iteration-p t)
    (release-resources game-object))
  (when (scene-background game-scene)
    (release-resources (scene-background game-scene)))
  (loop :for overlay :across (the (vector overlay) (slot-value game-scene 'scene-overlays)) :do
       (release-resources overlay)))

@export
(defgeneric add-to-scene (scene object)
  (:documentation "Add an object to the game scene")
  (:method ((scene scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (unless (find overlay scene-overlays)
        (vector-push-extend overlay scene-overlays)
        (push overlay (unloaded-game-objects scene)))))
  (:method ((scene game-scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (unless (find overlay scene-overlays)
        (vector-push-extend overlay scene-overlays)
        (push overlay (unloaded-game-objects scene)))))
  (:method ((scene game-scene) (object game-object))
    (when (start-tracking (spatial-partition scene) object)
      (add-subscriber object scene killed)
      (push object (unloaded-game-objects scene)))))

@export
(defgeneric remove-from-scene (scene object)
  (:documentation "Remove an object from the game scene")
  (:method ((scene scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (when (find overlay scene-overlays)
        (setf scene-overlays (delete overlay scene-overlays))
        (release-resources overlay))))
  (:method ((scene game-scene) (overlay overlay))
    (with-slots (scene-overlays) scene
      (when (find overlay scene-overlays)
        (setf scene-overlays (delete overlay scene-overlays))
        (render-queue-remove (slot-value scene 'render-queue) overlay)
        (release-resources overlay))))
  (:method ((scene game-scene) (object game-object))
    ;; remove object at the start of the next frame to allow pending actions to finish
    (remove-subscriber object scene killed)
    (stop-tracking (spatial-partition scene) object)
    (render-queue-remove (slot-value scene 'render-queue) object)
    (release-resources object)
    (vector-push-extend object (slot-value scene 'removed-objects))))

;; for subclasses to hook object updates
(defmethod found-object-to-update ((scene game-scene) game-object))

(defun %object-was-removed-p (scene object)
  (declare (optimize (speed 3)))
  (loop :for removed :across (the (vector game-object) (slot-value scene 'removed-objects)) :do
       (when (eq object removed)
         (return t))))

(defmethod update ((game-scene game-scene) delta-t-ms (null null))
  (declare (optimize (speed 3))
           (ignore null))
  (with-slots (update-area
               (queue render-queue)
               update-queue
               update-queue-fill-pointer
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
           (y-max (when update-area (active-area-max-y update-area))))
      (declare ((or null single-float) x-min x-max y-min y-max))
      (with-slots ((bg scene-background) scene-overlays) game-scene
        ;; reset rendering queue
        (render-queue-reset queue)
        ;; pre-update frame to mark positions
        (pre-update (camera game-scene))
        (when bg (pre-update bg))
        (loop :for overlay :across (the (vector overlay) scene-overlays) :do
             (pre-update overlay))
        ;; run scheduler
        (%run-scheduled-callbacks game-scene)
        ;; update frame
        (when bg
          (render-queue-add queue bg))
        (let* ((render-delta 16.0) ; TODO this value could be smaller
               (render-x-min (- (x camera) render-delta))
               (render-x-max (+ (x camera) (width camera) render-delta))
               (render-y-min (- (y camera) render-delta))
               (render-y-max (+ (y camera) (height camera) render-delta)))
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
              (when (and (in-render-area-p game-object)
                         (not (%object-was-removed-p game-scene game-object)))
                (render-queue-add queue game-object)))))
        (loop :for i :from 0 :below update-queue-fill-pointer :do
             (let ((game-object (elt update-queue i)))
               (found-object-to-update game-scene game-object)
               (update game-object delta-t-ms game-scene)))
        (loop :for overlay :across (the (vector overlay) scene-overlays) :do
             (update overlay delta-t-ms game-scene)
             (render-queue-add queue overlay))
        (update (camera game-scene) delta-t-ms game-scene)
        (when bg
          (update bg delta-t-ms game-scene))
        (incf (slot-value game-scene 'scene-ticks) delta-t-ms)
        (values)))))

(defmethod render ((game-scene game-scene) update-percent (camera simple-camera) renderer)
  (declare (optimize (speed 3)))
  (with-slots ((bg scene-background)
               spatial-partition
               unloaded-game-objects
               ;; TODO: queue renders during update iteration
               (queue render-queue)
               scene-overlays)
      game-scene
    (loop :while unloaded-game-objects :do
         (load-resources (pop unloaded-game-objects) renderer))
    (render queue update-percent camera renderer))
  (values))

(defevent-callback killed ((object obb) (game-scene game-scene))
  (remove-from-scene game-scene object))

@export
(defun schedule (game-scene timestamp zero-arg-fn)
  "When the value returned by SCENE-TICKS of GAME-SCENE equals or exceeds TIMESTAMP the ZERO-ARG-FN callback will be invoked."
  (declare (game-scene game-scene)
           (optimize (speed 3))
           )
  (with-slots ((tasks scheduled-tasks)) game-scene
    (declare (vector tasks))
    (loop :for i :from 0 :below (length tasks) :by 2 :do
         (unless (elt tasks i)
           (setf (elt tasks i) timestamp
                 (elt tasks (+ i 1)) zero-arg-fn)
           (return))
       :finally
         (vector-push-extend timestamp tasks)
         (vector-push-extend zero-arg-fn tasks))
    (values)))

@export
(defun cancel-scheduled-callback (zero-arg-fn &key (error-if-not-scheduled T))
  (declare (ignore zero-arg-fn error-if-not-scheduled))
  (error "TODO"))

(defun %run-scheduled-callbacks (game-scene)
  (declare (optimize (speed 3))
           (game-scene game-scene))
  (with-slots ((tasks scheduled-tasks) (now scene-ticks)) game-scene
    (declare (vector tasks))
    (loop :for i :from 0 :below (length tasks) :by 2 :do
         (when (elt tasks i)
           (let ((time-to-run (elt tasks i))
                 (callback (elt tasks (+ i 1))))
             (declare ((function ()) callback)
                      (fixnum time-to-run now))
             (when (>= now time-to-run)
               (funcall callback)
               (setf (elt tasks i) nil
                     (elt tasks (+ 1 i)) nil)))))
    (values)))

@export
(defun get-object-by-id (scene id)
  "Return the (presumably) unique game-object identified by ID in SCENE."
  (declare (game-scene scene))
  (block find-object
    (do-spatial-partition (game-object (spatial-partition scene) :static-iteration-p t)
      (when (equalp (object-id game-object) id)
        (return-from find-object game-object)))))
