(in-package :recurse.vert)

;; TODO: rename to timestep
(defconstant +update-timestep+ 16
  "Duration of the update timeslice in milliseconds. Just over 60 update frames per second.")

(defparameter *num-framerate-samples* 6
  "Number of render samples to keep when measuring FPS")
(defparameter *internal-real-time-type* (type-of (get-internal-real-time)))

(defparameter *dev-mode*
  nil
  "Set to non-nil to enable dev features (potentially at a cost to performance).")

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
                 ;; TODO: Use setf on active-scene instead of `change-scene` method
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
   (lag-ns :initform 0
           :accessor lag-ns)
   (last-loop-start-ns :initform (ticks-nanos) :accessor last-loop-start-ns)
   ;; debugging
   (fps-timer-start-ns :initform (ticks-nanos))
   (current-fps :initform 60.0)
   (fps-timer-duration-seconds :initform 4)
   (num-render-frames :initform 0))
  (:documentation "Starts and stops the game. Manages global engine state and services."))

(defevent engine-started ((engine-manager engine-manager))
    "Fired once when the engine starts running.")
(defevent engine-stopped ((engine-manager engine-manager))
    "Fired once when the engine stops running.")

(defgeneric change-scene (engine-manager new-scene &optional release-existing-scene preserve-audio)
  (:documentation "Replace the active-scene with NEW-SCENE.
If RELEASE-EXISTING-SCENE is non-nil (the default), the current active-scene will be released.")
  (:method ((engine-manager engine-manager) new-scene &optional (release-existing-scene T) (preserve-audio nil))
    (let ((old-scene (active-scene engine-manager)))
      (unless (eq old-scene new-scene)
        (when (and old-scene (current-music (audio-player engine-manager)))
          ;; Hack to resume music on unpause
          (unless preserve-audio
            (if release-existing-scene
                (setf (music-state (audio-player engine-manager)) :stopped)
                (setf (music-state (audio-player engine-manager)) :paused))))
        (when new-scene
          (load-resources new-scene (rendering-context engine-manager))
          (do-input-devices device (input-manager engine-manager)
            (add-scene-input new-scene device)))
        (when (and old-scene release-existing-scene)
          (release-resources old-scene))
        (setf (active-scene engine-manager) new-scene)
        (multiple-value-bind (width-px height-px)
            (window-size-pixels (application-window *engine-manager*))
          (%after-resize-window (application-window engine-manager) width-px height-px))
        (when (and (not preserve-audio)
                   (typep old-scene 'pause-scene)
                   (eq :paused (music-state (audio-player engine-manager))))
          (setf (music-state (audio-player engine-manager)) :playing)))
      new-scene)))

(defgeneric game-loop-iteration (engine-manager)
  (:documentation "Run a single iteration of the game loop.")
  (:method ((engine-manager engine-manager))
    (declare (optimize (speed 3)))
    (with-accessors ((active-scene active-scene)
                     (renderer rendering-context)
                     (lag lag-ns)
                     (last-loop-start last-loop-start-ns))
        engine-manager
      (declare (fixnum last-loop-start lag))
      (let* ((now-ns (ticks-nanos))
             (delta-ns (- (the fixnum now-ns) last-loop-start))
             (timestep-ns (* +update-timestep+ #.(expt 10 6))))
        (declare (fixnum now-ns delta-ns timestep-ns))
        (setf last-loop-start now-ns)
        (incf lag delta-ns)

        (when (>= lag (* 10 timestep-ns))
          ;; update thread was likely just blocked on debugging. Don't try to catch up.
          (format T "Update thread very far behind. Resetting to a good state.~%")
          (setf lag timestep-ns))
        (loop :while (>= lag timestep-ns) :do
             (decf lag timestep-ns)
             (update active-scene +update-timestep+ nil))

        (render active-scene
                (coerce (/ lag timestep-ns) 'single-float)
                nil
                renderer)
        (when *dev-mode* (dev-mode-pre-render engine-manager))
        (render-game-window engine-manager)
        (when *dev-mode* (dev-mode-post-game-loop-iteration engine-manager))))))

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
    (clear-all-caches *memory-manager*)
    (format t "~%~%")))

;; debugging and dev mode tools

(defun dev-mode-pre-render (engine-manager)
  (render-dev-mode-info engine-manager))

(defun dev-mode-post-game-loop-iteration (engine-manager)
  ;; (reload-textures-if-changed)
  (with-slots (num-render-frames) engine-manager
    (incf num-render-frames))
  (compute-framerate engine-manager))

(defun reload-textures-if-changed ()
  "If any textures have changed, flush the texture cache and reload."
  (declare (optimize (speed 3)))
  (flet ((reload-texture (objects-using-texture)
           (loop :for object :across (copy-seq objects-using-texture) :do
                (release-resources object))))
    (%do-cache (*resource-cache* path tex :mtime original-mtime :objects-using objects-using-texture)
      (declare (ignore tex))
      (let ((current-mtime (file-write-date path)))
        (declare (fixnum current-mtime original-mtime))
        (when (/= current-mtime original-mtime)
          (reload-texture objects-using-texture))))))

(defun compute-framerate (engine-manager)
  (declare (optimize (speed 3)))
  (with-slots (fps-timer-start-ns current-fps fps-timer-duration num-render-frames) engine-manager
    (let ((duration-seconds (/ (- (ticks-nanos) fps-timer-start-ns) (expt 10 9))))
      (when (> duration-seconds 2)
        (setf current-fps (/ (floor (* (/ num-render-frames (if (= 0 duration-seconds) 1 duration-seconds)) 100)) 100.0)
              fps-timer-start-ns (ticks-nanos)
              num-render-frames 0)))))

(let ((gc-count 0)
      (last-gc-time-ms 0)
      (gc-timer 0))
  (defun gc-callback ()
    (incf gc-count)
    (setf
     last-gc-time-ms (/ (- sb-ext:*gc-run-time* gc-timer) (/ internal-time-units-per-second 1000))
     gc-timer sb-ext:*gc-run-time*))

  (defun current-gc-count ()
    gc-count)

  (defun last-gc-time-ms ()
    last-gc-time-ms)
  #+sbcl
  (push #'gc-callback
        sb-ext:*after-gc-hooks*))

(let* ((line-width-px 150.0)
       (line-height-px 100.0)
       (rendered-text nil)
       (number-cache (make-instance 'cache
                                    :test #'equal
                                    :on-evict (lambda (line-num line-cache)
                                                (declare (ignore line-num))
                                                (clear-cache line-cache))))
       (last-known-gc-count (current-gc-count))
       (dynamic-use-measure-duration-ms 2000)
       (last-dynamic-use-measure (ticks))
       (dynamic-delta 0)
       (previous-dynamic-usage 0))

  (defmethod cleanup-engine :after (engine-manager)
    (clear-cache number-cache)
    (setf last-dynamic-use-measure 0)
    (when rendered-text
      (release-resources rendered-text)))

  (defun render-dev-mode-info (engine-manager)
    "Render dev-mode info to the upper-right corner of the game window"
    (declare (optimize (speed 3)))
    (unless rendered-text
      (setf rendered-text
            (make-instance 'font-drawable
                           :width line-width-px
                           :height line-height-px
                           :color *red*
                           :text "0.0fps")))
    (with-slots (current-fps) engine-manager
      (let ((camera (camera (active-scene engine-manager)))
            (line-num 0))
        (with-accessors ((camera-zoom scale)
                         (camera-x x)
                         (camera-y y)
                         (camera-width width)
                         (camera-height height))
            camera
          (with-accessors ((text-width width))
              rendered-text
            (declare (camera-scale camera-zoom)
                     (world-position camera-x camera-y)
                     (world-dimension camera-width camera-height text-width)
                     ((integer 0 50) line-num))
            (setf line-width-px (/ (screen-width (camera (active-scene *engine-manager*))) 10.0)
                  line-height-px (/ (screen-height (camera (active-scene *engine-manager*))) 10.0)
                  (width rendered-text) (/ line-width-px camera-zoom)
                  (height rendered-text) (/ line-height-px camera-zoom)
                  (x rendered-text) (- (+ camera-x camera-width) text-width))
            (flet ((render-debug-line (text)
                     (setf (text rendered-text) text
                           (y rendered-text) (+ camera-y (/ (* line-num line-height-px) camera-zoom)))
                     (render rendered-text
                             1.0
                             camera
                             (rendering-context engine-manager))
                     (incf line-num)))
              (let ((gc-count (current-gc-count)))
                (declare (fixnum gc-count last-known-gc-count))
                (when (> gc-count last-known-gc-count)
                  (setf last-known-gc-count gc-count)
                  ;; render a mostly green frame when gc occurs
                  (let ((clear-color (clear-color engine-manager)))
                    (setf (clear-color engine-manager) *green*)
                    (sdl2:render-clear (slot-value engine-manager 'rendering-context))
                    (setf (clear-color engine-manager) clear-color)))
                (render-debug-line (getcache-default current-fps
                                                     (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                     (format nil "~Afps" current-fps)))
                (render-debug-line (getcache-default gc-count
                                                     (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                     (format nil "GC# ~A" gc-count)))
                (let ((gc-time-ms (last-gc-time-ms)))
                  (render-debug-line (getcache-default gc-time-ms
                                                       (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                       (format nil "GC-MS: ~A" gc-time-ms))))
                (let* ((dynamic-use (/ (ceiling (the fixnum (sb-kernel:dynamic-usage)) #.(expt 10 5)) 10.0))
                       (now (ticks)))
                  (render-debug-line (getcache-default dynamic-use
                                                       (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                       (format nil "~Amb" dynamic-use)))
                  (when (>= (- now last-dynamic-use-measure) dynamic-use-measure-duration-ms)
                    (setf dynamic-delta (/ (round (* 1000 (/ (- dynamic-use previous-dynamic-usage) (/ (- now last-dynamic-use-measure) 1000)))) 1000.0))
                    (setf previous-dynamic-usage dynamic-use
                          last-dynamic-use-measure now))
                  (render-debug-line (getcache-default dynamic-delta
                                                       (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                       (format nil "~Amb/s" dynamic-delta))))))))))))

;;;; methods which will be provided by the implementation

(defgeneric quit-engine (engine-manager)
  (:documentation "Stop the running game engine"))

(defgeneric render-game-window (engine-manager)
  (:documentation "Render the current rendering context to the application-window."))

(defgeneric run-game-loop (engine-manager)
  (:documentation "Provided by engine-manager subclasses. Runs the game loop."))
