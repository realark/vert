(in-package :recurse.vert)

(defconstant +update-timestep+ (ceiling (/ 1000 60))
  "Duration of the update timeslice in milliseconds. Just over 60 update frames per second.")

(defclass engine-manager (event-publisher)
  ;; global services
  ((game-name :initform "Vert-Game"
              :reader game-name
              :initarg :game-name)
   (memory-manager :initform *memory-manager*
                   :reader memory-manager)
   (input-manager :initform (make-instance 'input-manager)
                  :reader input-manager)
   (application-window :initarg :application-window
                       :initform nil
                       :reader application-window
                       :documentation "Underlying application window.")
   (rendering-context :initarg :rendering-context
                      :initform nil
                      :reader rendering-context
                      :documentation "Rendering context of the application window.")
   (active-scene :initarg :active-scene
                 :initform nil
                 :accessor active-scene
                 :documentation "Scene to update and render in the game loop.")
   (audio-player :initarg :audio-player
                 :initform nil
                 :reader audio-player
                 :documentation "System audio player.")
   ;; engine state
   (clear-color :initform *black*
                :accessor clear-color
                :documentation "Set the clear color for the rendering window.")
   (next-update-timestamp :initarg :next-update-timestamp
                          :initform (ticks)
                          :accessor next-update-timestamp
                          :documentation "Timestamp (ms) of the next desired update frame.
Used to determine the number of update frames to execute and the set the interpolation value for the render frame."))
  (:documentation "Starts and stops the game. Manages global engine state and services."))

(defevent engine-started ((engine-manager engine-manager))
    "Fired once when the engine starts running.")
(defevent engine-stopped ((engine-manager engine-manager))
    "Fired once when the engine stops running.")

(defgeneric change-scene (engine-manager new-scene &optional release-existing-scene)
  (:documentation "Replace the active-scene with NEW-SCENE.
If RELEASE-EXISTING-SCENE is non-nil (the default), the current active-scene will be released.")
  (:method ((engine-manager engine-manager) new-scene &optional (release-existing-scene T))
    (let ((old-scene (active-scene engine-manager)))
      (unless (eq old-scene new-scene)
        (when (and old-scene (current-music (audio-player engine-manager)))
          ;; Hack to resume music on unpause
          (if release-existing-scene
              (setf (music-state (audio-player engine-manager)) :stopped)
              (setf (music-state (audio-player engine-manager)) :paused)))
        (when new-scene
          (load-resources new-scene (rendering-context engine-manager))
          (do-input-devices device (input-manager engine-manager)
            (add-scene-input new-scene device))
          (setf (next-update-timestamp engine-manager) nil))
        (when (and old-scene release-existing-scene)
          (release-resources old-scene))
        (setf (active-scene engine-manager) new-scene)
        (multiple-value-bind (width-px height-px)
            (window-size-pixels (application-window *engine-manager*))
          (%after-resize-window (application-window engine-manager) width-px height-px))
        (when (and (typep old-scene 'pause-scene)
                   (eq :paused (music-state (audio-player engine-manager))))
          (setf (music-state (audio-player engine-manager)) :playing)))
      new-scene)))

(defmethod (setf next-update-timestamp) :around (value (engine-manager engine-manager))
  (if value
      (call-next-method value engine-manager)
      (call-next-method (ticks) engine-manager)))

(defgeneric game-loop-iteration (engine-manager)
  (:documentation "Run a single iteration of the game loop.")
  (:method ((engine-manager engine-manager))
    (declare (optimize (space 3)))
    (with-accessors ((active-scene active-scene)
                     (renderer rendering-context)
                     (next-update-timestamp next-update-timestamp))
        engine-manager
      (let ((start-of-loop-ts (ticks)))
        (declare
         ((integer 1 100) +update-timestep+)
         (fixnum next-update-timestamp start-of-loop-ts))
        ;; update
        (loop with i = 0
           while (<= next-update-timestamp start-of-loop-ts) do
             (locally
                 (declare ((integer 0 11) i))
               (when (>= (incf i) 10)
                 ;; after 10 iterations something is really wrong (maybe slow hardware).
                 ;; Give up trying to catch up and just reset to a normal state.
                 (setf next-update-timestamp (+ start-of-loop-ts +update-timestep+))
                 (return)))
             (update active-scene +update-timestep+ nil)
             (incf next-update-timestamp +update-timestep+))
        ;; render
        (render active-scene
                (coerce
                 (/ (- (+ start-of-loop-ts +update-timestep+) next-update-timestamp)
                    +update-timestep+)
                 'single-float)
                nil
                renderer)
        (render-game-window engine-manager)
        ;; FIXME sleep if no vsync
        (when *dev-mode* (dev-mode-post-game-loop-iteration))))))

(defgeneric run-game (engine-manager initial-scene-creator)
  (:documentation "Initialize ENGINE-MANAGER and start the game.
The INITIAL-SCENE-CREATOR (zero-arg function) returns the first scene of the game.
It is invoked after the engine is fully started.")
  (:method ((engine-manager engine-manager) initial-scene-creator)
    (unwind-protect
         (progn
           (clear-cache *resource-cache*)
           ;; start services
           (setf (slot-value engine-manager 'audio-player)
                 (start-audio-system))
           ;; fire events
           (fire-event engine-manager engine-started)
           ;; set up initial active-scene
           (change-scene engine-manager (funcall initial-scene-creator))
           (sb-ext:gc :full T) ;; run a full gc before the first window is shown
           ;; run the game loop
           (run-game-loop engine-manager))
      (cleanup-engine engine-manager))))

(defgeneric cleanup-engine (engine-manager)
  (:documentation "Called once at engine shutdown. Clean up engine state.")
  (:method ((engine-manager engine-manager))
    (change-scene engine-manager nil)
    ;; stop services
    (stop-audio-system)
    (fire-event engine-manager engine-stopped)
    (format t "~%~%")))

;;;; methods which will be provided by the implementation

(defgeneric quit-engine (engine-manager)
  (:documentation "Stop the running game engine"))

(defgeneric render-game-window (engine-manager)
  (:documentation "Render the current rendering context to the application-window."))

(defgeneric run-game-loop (engine-manager)
  (:documentation "Provided by engine-manager subclasses. Runs the game loop."))
