(in-package :recurse.vert)

@export
(defvar *engine-manager*
  nil
  "State used to run the game loop")

@export
(defvar *scene*
  nil
  "The active scene being updated")

(declaim ((integer 1 100) *timestep*))
@export
(defvar *timestep*
  24
  "Duration of the update timeslice in milliseconds.")

(defclass engine-manager (event-publisher)
  ;; global services
  ((input-manager :initform (make-instance 'input-manager)
                  :reader input-manager)
   (application-window :initarg :application-window
                       :initform nil
                       :reader application-window
                       :documentation "Underlying application window.")
   (rendering-context :initarg :rendering-context
                      :initform nil
                      :reader rendering-context
                      :documentation "Rendering context of the application window.")
   (audio-player :initarg :audio-player
                 :initform nil
                 :reader audio-player
                 :documentation "System audio player.")
   (pending-scene-changes :initform nil)
   (pending-action :initform 'no-action
                   :documentation "A zero-arg lambda to execute at the end of the next game loop iteration")
   (clear-color :initform *black*
                :accessor clear-color
                :documentation "Set the clear color for the rendering window.")
   (lag-ns :initform 0
           :accessor lag-ns)
   (last-loop-start-ns :initform (ticks-nanos) :accessor last-loop-start-ns)
   (game-stats :initform (make-array 1
                                     :element-type 'game-stats
                                     :initial-element (make-instance 'builtin-vert-stats)
                                     :fill-pointer 1
                                     :adjustable t)
               :reader game-stats)
   (stats-hud :initform nil :accessor stats-hud)
   ;; stats and debugging
   (update-time-nanos :initform 0)
   (num-updates :initform 0)
   (render-time-nanos :initform 0)
   (fps-timer-start-ns :initform (ticks-nanos))
   (current-fps :initform 60.0)
   (fps-timer-duration-seconds :initform 4)
   (num-render-frames :initform 0)
   (avg-update-time :initform 0)
   (avg-render-time :initform 0))
  (:documentation "Starts and stops the game. Manages global engine state and services."))
(export '(rendering-context))

(defmethod initialize-instance :after ((engine-manager engine-manager) &rest args)
  (declare (ignore args))
  (with-slots (game-stats stats-hud) engine-manager
    (setf stats-hud (make-instance 'stats-hud
                                   :game-stats game-stats
                                   :width (first (getconfig 'game-resolution *config*))
                                   :height (second (getconfig 'game-resolution *config*))))))

(defgeneric quit-engine (engine-manager)
  (:documentation "Stop the running game engine"))

(defgeneric render-game-window (engine-manager)
  (:documentation "Render the current rendering context to the application-window."))

(defgeneric run-game-loop (engine-manager)
  (:documentation "Provided by engine-manager subclasses. Runs the game loop."))

(defevent engine-started ((engine-manager engine-manager))
    "Fired once when the engine starts running.")
(defevent engine-stopped ((engine-manager engine-manager))
    "Fired once when the engine stops running.")

(defun active-scene (engine-manager)
  (declare (ignore engine-manager))
  *scene*)

@export
(defun change-scene (engine-manager new-scene &optional (release-existing-scene T) (preserve-audio nil))
  "At the beginning of the next game loop, Replace the active-scene with NEW-SCENE.
If RELEASE-EXISTING-SCENE is non-nil (the default), the current active-scene will be released."
  (let ((old-scene *scene*))
    (unless (eq old-scene new-scene)
      (when (slot-value engine-manager 'pending-scene-changes)
        (error "Scene change already pending"))
      (setf (slot-value engine-manager 'pending-scene-changes)
            (lambda ()
              (when (and old-scene (current-music *audio*))
                ;; Hack to resume music on unpause
                (unless preserve-audio
                  (if release-existing-scene
                      (setf (music-state *audio*) :stopped)
                      (setf (music-state *audio*) :paused))))
              (when new-scene
                (load-resources new-scene (rendering-context engine-manager))
                (do-input-devices device (input-manager engine-manager)
                  (add-scene-input new-scene device)))
              (when (and old-scene release-existing-scene)
                (release-resources old-scene))
              (setf *scene* new-scene)
              (multiple-value-bind (width-px height-px)
                  (window-size-pixels (application-window *engine-manager*))
                (%after-resize-window (application-window engine-manager) width-px height-px))
              (when (and preserve-audio
                         (typep old-scene 'pause-scene)
                         (eq :paused (music-state *audio*)))
                (setf (music-state *audio*) :playing)))))
    (values)))

(defgeneric game-loop-iteration (engine-manager)
  (:documentation "Run a single iteration of the game loop.")
  (:method ((engine-manager engine-manager))
    (declare (optimize (speed 3)))
    ;; first run any pending scene changes
    (with-slots (pending-scene-changes) engine-manager
      (when pending-scene-changes
        (funcall pending-scene-changes)
        (setf pending-scene-changes nil)))
    ;; then any pending dev actions
    (with-slots (pending-action) engine-manager
      (unless (eq 'no-action pending-action)
        (let ((pending pending-action))
          (declare (function pending))
          (sb-ext:atomic-update pending-action
                                (lambda (previous-action)
                                  (declare (ignore previous-action))
                                  'no-action))
          (funcall pending))))
    ;; I've considered wrapping this methig in (sb-sys:without-gcing)
    ;; to prevent short GCs from dropping the FPS.
    ;; But it seems like that could deadlock if the game is multithreaded.
    (with-accessors ((active-scene active-scene)
                     (renderer rendering-context)
                     (lag lag-ns)
                     (game-stats game-stats)
                     (stats-hud stats-hud)
                     (last-loop-start last-loop-start-ns))
        engine-manager
      (declare (fixnum last-loop-start lag))
      (let* ((now-ns (ticks-nanos))
             (delta-ns (- (the fixnum now-ns) last-loop-start))
             (timestep-ns (* *timestep* #.(expt 10 6)))
             (num-updates 0)
             (update-time-nanos 0)
             (render-time-nanos 0))
        (declare (fixnum now-ns delta-ns timestep-ns num-updates update-time-nanos render-time-nanos))
        (setf last-loop-start now-ns)
        (incf lag delta-ns)

        (when (>= lag (* 10 timestep-ns))
          ;; update thread was likely just blocked on debugging. Don't try to catch up.
          (format T "Update thread very far behind. Resetting to a good state.~%")
          (setf lag timestep-ns))
        (loop :for i :from 0 :while (>= lag timestep-ns) :do
             (decf lag timestep-ns)
             (let ((t0 (ticks-nanos)))
               (declare (fixnum t0))
               (when (get-dev-config 'dev-mode-performance-hud)
                 (loop :for stats :across game-stats :do
                      (when (typep stats 'builtin-vert-stats)
                        (pre-update-frame stats))))
               (update active-scene *timestep* nil)
               (when (get-dev-config 'dev-mode-performance-hud)
                 (loop :for stats :across game-stats :do
                      (when (typep stats 'builtin-vert-stats)
                        (post-update-frame stats))))
               (incf update-time-nanos (the fixnum (- (the fixnum (ticks-nanos)) t0)))
               (incf num-updates)))

        (let ((t0 (ticks-nanos)))
          (declare (fixnum t0))
          (when (get-dev-config 'dev-mode-performance-hud)
            (loop :for stats :across game-stats :do
                 (when (typep stats 'builtin-vert-stats)
                   (pre-render-frame stats))))
          (render active-scene
                  (coerce (/ lag timestep-ns) 'single-float)
                  nil
                  renderer)
          (when (get-dev-config 'dev-mode-performance-hud)
            (loop :for stats :across game-stats :do
                 (when (typep stats 'builtin-vert-stats)
                   (post-render-frame stats))))
          (incf render-time-nanos (the fixnum (- (the fixnum (ticks-nanos)) t0))))
        (when (get-dev-config 'dev-mode-performance-hud)
          (dev-mode-pre-render engine-manager))
        (when (get-dev-config 'dev-mode-performance-hud)
          (render stats-hud 0 (camera *scene*) renderer))
        (render-game-window engine-manager)
        (when (get-dev-config 'dev-mode-performance-hud)
          (dev-mode-post-game-loop-iteration engine-manager  update-time-nanos num-updates render-time-nanos))))))

(defgeneric run-game (engine-manager initial-scene-creator)
  (:documentation "Initialize ENGINE-MANAGER and start the game.
The INITIAL-SCENE-CREATOR (zero-arg function) returns the first scene of the game.
It is invoked after the engine is fully started.")
  (:method ((engine-manager engine-manager) initial-scene-creator)
    (unwind-protect
         (progn
           (do-cache (*engine-caches* cache-name cache)
             (clear-cache cache))
           ;; start services
           (setf (slot-value engine-manager 'audio-player)
                 (start-audio-system))
           ;; fire events
           (fire-event engine-manager engine-started)
           ;; set up initial active-scene
           (change-scene engine-manager (funcall initial-scene-creator))
           (loop :for label :being :the hash-keys :of *engine-start-hooks*
              :using (hash-value hook)
              :do (funcall hook))
           (sb-ext:gc :full T) ;; run a full gc before the first window is shown
           ;; run the game loop
           (run-game-loop engine-manager))
      (format t "Game Complete. Cleaning up Engine.~%")
      (cleanup-engine engine-manager))))

(defgeneric cleanup-engine (engine-manager)
  (:documentation "Called once at engine shutdown. Clean up engine state.")
  (:method ((engine-manager engine-manager))
    (change-scene engine-manager nil)
    (with-slots (pending-scene-changes) engine-manager
      (when pending-scene-changes
        (funcall pending-scene-changes)
        (setf pending-scene-changes nil)))
    ;; stop services
    (stop-audio-system)
    (fire-event engine-manager engine-stopped)
    (loop :for label :being :the hash-keys :of *engine-stop-hooks*
       :using (hash-value hook)
       :do (funcall hook))
    (do-cache (*engine-caches* cache-name cache)
      (clear-cache cache))
    (format t "~%~%")))

;; debugging and dev mode tools

(defun dev-mode-pre-render (engine-manager)
  (render-dev-mode-info engine-manager))

(defun dev-mode-post-game-loop-iteration (engine-manager update-time-nanos num-updates render-time-nanos)
  ;; (reload-textures-if-changed)
  (with-slots ((total-update-time update-time-nanos)
               (total-num-updates num-updates)
               (total-render-time render-time-nanos)
               (total-render-frames num-render-frames))
      engine-manager
    (declare (fixnum update-time-nanos
                     num-updates
                     render-time-nanos
                     total-update-time
                     total-num-updates
                     total-render-time
                     total-render-frames))
    (incf total-update-time update-time-nanos)
    (incf total-num-updates num-updates)
    (incf total-render-time render-time-nanos)
    (incf total-render-frames))
  (compute-stats engine-manager))

(defun reload-textures-if-changed ()
  "If any textures have changed, flush the texture cache and reload."
  (declare (optimize (speed 3)))
  (flet ((reload-texture (objects-using-texture)
           (loop :for object :across (copy-seq objects-using-texture) :do
                (release-resources object))))
    ;; TODO: clear texture and audio caches
    #+nil
    (do-cache-with-metadata (*resource-cache* path tex :mtime original-mtime :objects-using objects-using-texture)
      (declare (ignore tex))
      (let ((current-mtime (file-write-date path)))
        (declare (fixnum current-mtime original-mtime))
        (when (/= current-mtime original-mtime)
          (reload-texture objects-using-texture))))))

(defun compute-stats (engine-manager)
  (declare (optimize (speed 3)))
  (with-slots (fps-timer-start-ns
               current-fps
               fps-timer-duration
               render-time-nanos
               num-render-frames
               update-time-nanos
               num-updates
               avg-update-time
               avg-render-time)
      engine-manager
    (let ((duration-seconds (/ (- (ticks-nanos) fps-timer-start-ns) (expt 10 9))))
      (when (> duration-seconds 2)
        (setf current-fps (/ (floor (* (/ num-render-frames (if (= 0 duration-seconds) 1 duration-seconds)) 100)) 100.0)
              avg-render-time (round (/ render-time-nanos #.(expt 10 6)) num-render-frames)
              avg-update-time (round (/ update-time-nanos #.(expt 10 6) num-updates))
              fps-timer-start-ns (ticks-nanos)
              update-time-nanos 0
              num-updates 0
              render-time-nanos 0
              num-render-frames 0)))))

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
      (release-resources rendered-text)
      (setf rendered-text nil)))

  (defun render-dev-mode-info (engine-manager)
    "Render dev-mode info to the upper-right corner of the game window"
    (declare (optimize (speed 3)))
    (unless rendered-text
      (setf rendered-text
            (make-instance 'font-drawable
                           :width line-width-px
                           :height line-height-px
                           :color *red*
                           :text "0.0fps"))
      (load-resources rendered-text (rendering-context engine-manager)))
    (with-slots (current-fps
                 avg-render-time
                 avg-update-time)
        engine-manager
      (let ((camera (camera (active-scene engine-manager)))
            (line-num 0))
        (with-accessors ((camera-x x)
                         (camera-y y)
                         (camera-width width)
                         (camera-height height))
            camera
          (with-accessors ((text-width width))
              rendered-text
            (declare (world-position camera-x camera-y)
                     (world-dimension camera-width camera-height text-width)
                     ((integer 0 50) line-num))
            (setf line-width-px (/ (width (camera (active-scene *engine-manager*))) 10.0)
                  line-height-px (/ (height (camera (active-scene *engine-manager*))) 10.0)
                  (width rendered-text) line-width-px
                  (height rendered-text) line-height-px
                  (x rendered-text) (- (+ camera-x camera-width) text-width))
            (flet ((render-debug-line (text)
                     (setf (text rendered-text) text
                           (y rendered-text) (+ camera-y (* line-num line-height-px)))
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
                    (gl:clear :depth-buffer-bit :color-buffer-bit)
                    (setf (clear-color engine-manager) clear-color)))
                (render-debug-line (getcache-default current-fps
                                                     (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                     (format nil "~Afps" current-fps)))
                (render-debug-line (getcache-default avg-render-time
                                                     (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                     (format nil "r: ~Ams" avg-render-time)))
                (render-debug-line (getcache-default avg-update-time
                                                     (getcache-default line-num number-cache (make-instance 'cache :test #'equalp))
                                                     (format nil "u: ~Ams" avg-update-time)))
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

;;;; on-game-thread macro

@export
(defmacro on-game-thread (&body body)
  "Run BODY on vert's game-thread at the end of the next loop iteration."
  `(if (equal (bt:thread-name (bt:current-thread))
              +game-loop-thread-name+)
       (progn ,@body)
       (unless (eq 'no-action
                   (sb-ext:compare-and-swap (slot-value *engine-manager* 'pending-action)
                                            'no-action
                                            (lambda () ,@body)))
         (error "Action already pending"))))
