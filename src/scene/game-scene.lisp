(in-package :recurse.vert)

@export-class
(defclass static-object ()
  ()
  (:documentation "A marker class which tells a game-scene that the object does not require updates.
This is an optimization to allow scenes to skip updates for objects."))

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
   (spatial-partition :initform nil
                      :reader spatial-partition))
  (:documentation "A game world."))

(defmethod initialize-instance :after ((game-scene game-scene) &rest args)
  (declare (ignore args))
  (with-slots (spatial-partition) game-scene
    (setf spatial-partition
          (make-instance 'layered-quadtree :max-depth 20
                         :max-objects 10
                         :width (width game-scene)
                         :height (height game-scene)))))

(defmethod load-resources ((game-scene game-scene) renderer)
  (with-accessors ((music scene-music)
                   (bg scene-background))
      game-scene
    (when bg
      (load-resources bg renderer))
    (do-spatial-partition (game-object (spatial-partition game-scene))
      (load-resources game-object renderer))
    (when music
      ;; Hack to resume music on unpause
      (if (eq :paused (music-state *audio*))
          (setf (music-state *audio*) :playing)
          (play-music *audio* music :num-plays -1)))))

(defmethod release-resources ((game-scene game-scene))
  (do-spatial-partition (game-object (spatial-partition game-scene))
    (remove-from-scene game-scene game-object))
  (when (scene-background game-scene)
    (release-resources (scene-background game-scene))))

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
    (unless (find-spatial-partition object (spatial-partition scene))
      (add-subscriber object scene killed)
      (start-tracking (spatial-partition scene) object)
      (push object (unloaded-game-objects scene)))))

(defmethod add-to-scene :after ((scene game-scene) (object-manager object-manager))
  (loop :for managed-obj :in (get-managed-objects object-manager) :do
       (add-to-scene scene managed-obj)))

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
        (release-resources overlay))))
  (:method ((scene game-scene) (object game-object))
    (stop-tracking (spatial-partition scene) object)
    (release-resources object)))

(defmethod remove-from-scene :after ((scene game-scene) (object-manager object-manager))
  (loop :for managed-obj :in (get-managed-objects object-manager) :do
       (remove-from-scene scene managed-obj)))

(defmethod update ((game-scene game-scene) delta-t-ms (null null))
  (declare (optimize (speed 3))
           (ignore null))
  (with-slots ((bg scene-background) scene-overlays) game-scene
    (when bg (pre-update bg))
    (do-spatial-partition (game-object (spatial-partition game-scene))
      (unless (typep game-object 'static-object)
        (pre-update game-object)))
    (loop :for overlay :across (the (vector overlay) scene-overlays) :do
         (pre-update overlay))
    (pre-update (camera game-scene))
    (%run-scheduled-callbacks game-scene)
    (do-spatial-partition (game-object (spatial-partition game-scene))
      (unless (typep game-object 'static-object)
        (update game-object delta-t-ms game-scene)))
    ;; update camera first in case overlay or bg to use the camera's position
    (update (camera game-scene) delta-t-ms game-scene)
    (loop :for overlay :across (the (vector overlay) scene-overlays) :do
         (update overlay delta-t-ms game-scene))
    (when bg (update bg delta-t-ms game-scene))
    (incf (slot-value game-scene 'scene-ticks) delta-t-ms)
    (values)))

(defmethod render ((game-scene game-scene) update-percent (camera simple-camera) renderer)
  (declare (optimize (speed 3)))
  (with-slots ((bg scene-background)
               spatial-partition
               unloaded-game-objects
               scene-overlays)
      game-scene
    (when bg
      (render bg update-percent camera renderer))
    (loop :while unloaded-game-objects :do
         (load-resources (pop unloaded-game-objects) renderer))
    ;; TODO: put this check inside the object being rendered
    (flet ((in-camera (camera game-object)
             (declare (obb camera game-object))
             (with-accessors ((x1 x) (y1 y) (z1 z)
                              (w1 width) (h1 height))
                 camera
               (with-accessors ((x2 x) (y2 y) (z2 z)
                                (w2 width) (h2 height))
                   game-object
                 (declare (world-dimension w1 h1 w2 h2)
                          (world-position x1 y1 z1 x2 y2 z2))
                 (let ((delta 10))
                   ;; fudge the check a bit to account for rendering interpolation
                   (and (< (- x1 delta) (+ x2 w2))
                        (> (+ x1 w1 delta) x2)
                        (< (- y1 delta) (+ y2 h2))
                        (> (+ h1 y1 delta) y2)))))))
      (declare (inline in-camera))
      (do-spatial-partition (game-object spatial-partition)
        (when (in-camera camera game-object)
          (render game-object update-percent camera renderer))))
    (loop :for overlay :across (the (vector overlay) scene-overlays) :do
         (render overlay update-percent camera renderer)))
  (values))

(defevent-callback killed ((object obb) (game-scene game-scene))
  (remove-from-scene game-scene object))

@export
(defun schedule (game-scene timestamp zero-arg-fn)
  "When the value returned by SCENE-TICKS of GAME-SCENE equals or exceeds TIMESTAMP the ZERO-ARG-FN callback will be invoked."
  (declare (game-scene game-scene))
  (with-slots (scheduled-tasks) game-scene
    (vector-push-extend timestamp scheduled-tasks)
    (vector-push-extend zero-arg-fn scheduled-tasks)))

@export
(defun cancel-scheduled-callback (zero-arg-fn &key (error-if-not-scheduled T))
  (declare (ignore zero-arg-fn error-if-not-scheduled))
  (error "TODO"))

(defun %run-scheduled-callbacks (game-scene)
  (declare (game-scene game-scene))
  (with-slots ((tasks scheduled-tasks) (now scene-ticks)) game-scene
    (loop :for i :from 0 :below (length tasks) :by 2 :do
         (let ((time-to-run (elt tasks i))
               (callback (elt tasks (+ i 1))))
           (if (>= now time-to-run)
               (progn
                 (funcall callback)
                 (setf (elt tasks i) nil
                       (elt tasks (+ 1 i)) nil))
               ;; timestamps are ordered so we know nothing else is scheduled
               (return))))
    (setf tasks (delete nil tasks))))

@export
(defun get-object-by-id (scene id)
  "Return the (presumably) unique game-object identified by ID in SCENE."
  (declare (game-scene scene)
           (integer id))
  (block find-object
    (do-spatial-partition (game-object (spatial-partition scene))
      (when (equalp (object-id game-object) id)
        (return-from find-object game-object)))))
