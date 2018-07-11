(in-package :recurse.vert)

(defclass game-scene (scene)
  ((scene-background :initform nil
                     :initarg :background
                     :type scene-background
                     :accessor scene-background)
   (scene-ticks :initform 0
                :reader scene-ticks
                :documentation "Amount of milliseconds passed in the game scene")
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

;; TODO: store an audio player in scene instead of using engine-manager
(defun %get-audio-player ()
  (when *engine-manager*
    (audio-player *engine-manager*)))

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
      (if (eq :paused (music-state (%get-audio-player)))
          (setf (music-state (%get-audio-player)) :playing)
          (play-music (%get-audio-player) music :num-plays -1)))))

(defmethod release-resources ((game-scene game-scene))
  (do-spatial-partition (game-object (spatial-partition game-scene))
    (remove-from-scene game-scene game-object))
  (when (scene-background game-scene)
    (release-resources (scene-background game-scene))))

(defgeneric add-to-scene (scene object)
  (:documentation "Add an object to the game scene")
  (:method ((scene game-scene) (object game-object))
    (unless (find-spatial-partition object (spatial-partition scene))
      (add-subscriber object scene killed)
      (start-tracking (spatial-partition scene) object)
      (push object (unloaded-game-objects scene)))))

(defmethod add-to-scene :after ((scene game-scene) (object-manager object-manager))
  (loop for managed-obj in (get-managed-objects object-manager) do
       (add-to-scene scene managed-obj)))

(defgeneric remove-from-scene (scene object)
  (:documentation "Remove an object from the game scene")
  (:method ((scene game-scene) (object game-object))
    (stop-tracking (spatial-partition scene) object)
    (release-resources object)))

(defmethod update ((game-scene game-scene) delta-t-ms (null null))
  (declare (optimize (speed 3))
           (ignore null))
  (do-spatial-partition (game-object (spatial-partition game-scene))
    (update game-object delta-t-ms game-scene))
  (incf (slot-value game-scene 'scene-ticks) delta-t-ms)
  (values))

(defmethod render ((game-scene game-scene) update-percent (camera simple-camera) renderer)
  (declare (optimize (speed 3)))
  (with-slots ((bg scene-background)
               (unloaded-game-objects unloaded-game-objects))
      game-scene
    (when bg
      (render bg update-percent camera renderer))
    (loop while unloaded-game-objects do
         (load-resources (pop unloaded-game-objects) renderer))
    (do-spatial-partition (game-object (spatial-partition game-scene))
      (render game-object update-percent camera renderer)))
  (values))

(defevent-callback killed ((object aabb) (game-scene game-scene))
  (remove-from-scene game-scene object))
