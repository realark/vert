(in-package :recurse.vert)

;;;; game-stats interface
@export
(defconstant +default-stats-decimal-places+ 2)

@export-class
(defclass game-stats ()
  ()
  (:documentation "Dev tool. Computes and renders statistics which are rendered to the screen when dev-mode is enabled."))

@export
(defun add-game-stats (engine-manager game-stats)
  (unless (find game-stats (slot-value engine-manager 'game-stats))
    (vector-push-extend game-stats (slot-value engine-manager 'game-stats))))

@export
(defun remove-game-stats (engine-manager game-stats)
  (when (find game-stats (slot-value engine-manager 'game-stats))
    (setf (slot-value engine-manager 'game-stats) (delete game-stats (slot-value engine-manager 'game-stats)))))

(defgeneric get-stats-strings (game-stats)
  (:documentation "Return an array of strings for all stats and reset GAME-STATS timers if appropriate. Each entry will be rendered per line of dev-mode info.")
  (:method ((game-stats game-stats)) (make-array 0 :element-type 'string)))

;;;; stats which measure some type of framerate
(defclass frame-timer (game-stats)
  ((prefix :initarg :prefix :initform "")
   (stats-array :initform (make-array 2 :element-type 'string :initial-element ""))
   (timer-start-timestamp :initform (ticks-nanos)
                          :documentation "timestamp of when this frame timer began to measure frames.")
   (frame-start-timestamp :initform nil
                          :documentation "timestamp of when the last frame being measure began.")
   (sum-frames :initform 0
               :documentation "Total frame time (nanoseconds)")
   (num-frames :initform 0
               :documentation "Number of frames seen")))

(defun frame-timer-start-timer (frame-timer)
  (declare (optimize (speed 3)))
  (with-slots (frame-start-timestamp) frame-timer
    (setf frame-start-timestamp (ticks-nanos))))

(defun frame-timer-stop-timer (frame-timer)
  (declare (optimize (speed 3)))
  (with-slots (frame-start-timestamp sum-frames num-frames) frame-timer
    (when frame-start-timestamp
      (locally (declare (fixnum frame-start-timestamp num-frames sum-frames))
        (let ((frame-time (- (the fixnum (ticks-nanos)) frame-start-timestamp)))
          (declare (fixnum frame-time))
          (incf sum-frames frame-time)
          (incf num-frames))))))

(defun frame-timer-stats (frame-timer)
  "Get stats for the time period covering the last reset for FRAME-TIMER to now. (values avg-frame-time-ns frames-per-second)"
  (declare (optimize (speed 3)))
  (with-slots (timer-start-timestamp frame-start-timestamp sum-frames num-frames) frame-timer
    (declare (fixnum timer-start-timestamp sum-frames num-frames))
    (if (= 0 num-frames)
        (values 0 0.0)
        (let* ((delta-t-ns (- (the fixnum (ticks-nanos)) timer-start-timestamp))
               (avg-frame-time-ns (/ sum-frames num-frames))
               (fps (/ num-frames (/ (the fixnum delta-t-ns) #.(expt 10.0 9)))))
          (values avg-frame-time-ns fps)))))

(defun frame-timer-reset (frame-timer)
  (with-slots (timer-start-timestamp frame-start-timestamp sum-frames num-frames) frame-timer
    (setf timer-start-timestamp (ticks-nanos)
          frame-start-timestamp nil
          sum-frames 0
          num-frames 0)
    frame-timer))

(defmethod get-stats-strings ((frame-timer frame-timer))
  (multiple-value-bind (avg-frame-time-ns fps)
      (frame-timer-stats frame-timer)
    (prog1
        (with-slots (stats-array prefix) frame-timer
          (setf (elt stats-array 0) (format nil "~Afps: ~A" prefix (truncate-float fps +default-stats-decimal-places+))
                (elt stats-array 1) (format nil "~Aavg: ~Ams" prefix (truncate-float (/ avg-frame-time-ns #.(expt 10.0 6))
                                                                                     +default-stats-decimal-places+)))
          stats-array)
      (frame-timer-reset frame-timer))))

;;;; GC stats utils
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

;;;; builtin vert stats
(defclass builtin-vert-stats (game-stats)
  ((next-refresh-timestamp-ms :initform 0
                              :documentation "timestamp (ms) of when stats should be refreshed.")
   (refresh-frequency-ms :initarg :refresh-frequency-ms
                         :initform 1000)
   (stats-array :initform (make-array 8 :element-type 'string :initial-element ""))
   (render-timer :initform (make-instance 'frame-timer :prefix "r "))
   (update-timer :initform (make-instance 'frame-timer :prefix "u "))
   (last-measured-heap-size :initform (sb-kernel:dynamic-usage)))
  (:documentation "Game-stats which is invoked at specific times by the engine-manager."))

(defun pre-update-frame (vert-stats)
  (with-slots (update-timer) vert-stats
    (frame-timer-start-timer update-timer)))

(defun post-update-frame (vert-stats)
  (with-slots (update-timer) vert-stats
    (frame-timer-stop-timer update-timer)))

(defun pre-render-frame (vert-stats)
  (with-slots (render-timer) vert-stats
    (frame-timer-start-timer render-timer)))

(defun post-render-frame (vert-stats)
  (with-slots (render-timer) vert-stats
    (frame-timer-stop-timer render-timer)))

(defun compute-memory-stats (vert-stats)
  (with-slots (gc-count last-gc-duration-ms heap-size-mb mb-per-second) vert-stats
    (setf gc-count (gc-count)
          last-gc-duration-ms (last-gc-time-ms))
    ))

(defmethod get-stats-strings ((builtin-stats builtin-vert-stats))
  (declare (optimize (speed 3)))
  (with-slots (next-refresh-timestamp-ms refresh-frequency-ms stats-array)
      builtin-stats
    (let ((now (ticks)))
      (when (>= now next-refresh-timestamp-ms)
        (setf next-refresh-timestamp-ms (+ now refresh-frequency-ms))
        (let ((index 0))
          (loop :for timer-stat :across (get-stats-strings (slot-value builtin-stats 'render-timer)) :do
               (setf (elt stats-array index) timer-stat)
               (incf index))
          (loop :for timer-stat :across (get-stats-strings (slot-value builtin-stats 'update-timer)) :do
               (setf (elt stats-array index) timer-stat)
               (incf index))
          (setf (elt stats-array index) (format nil "GC#: ~A" (current-gc-count)))
          (incf index)
          (setf (elt stats-array index) (format nil "GC-ms: ~A" (last-gc-time-ms)))
          (incf index)

          (with-slots (last-measured-heap-size) builtin-stats
            (let* ((current-heap-size (sb-kernel:dynamic-usage))
                   (current-heap-size-mb (truncate-float (/ (sb-kernel:dynamic-usage) #.(expt 10.0 6)) 1))
                   (heap-delta (- current-heap-size last-measured-heap-size))
                   (seconds-since-last-measure (/ refresh-frequency-ms 1000.0)))
              (setf last-measured-heap-size current-heap-size)
              (setf (elt stats-array index) (format nil "heap-mb: ~A" current-heap-size-mb))
              (incf index)
              (setf (elt stats-array index) (format nil "mb/s: ~A" (truncate-float (/ heap-delta #.(expt 10.0 6)) 3)))
              (incf index))))))
    stats-array))

;;;; dev-mode hud

(defclass stats-hud (overlay)
  ((line-width :initform 50.0)
   (line-height :initform 30.0)
   (game-stats :initarg :game-stats :initform (error ":game-stats required"))
   (text-color :initform (make-color-rgba :r 255))
   (font-drawable :initform nil))
  (:documentation ""))

(defun %render-stats-hud (hud rendering-context)
  (declare (optimize (speed 3)))
  (let ((children (transform-children hud))
        (index 0))
    (declare ((integer 0 1000) index))
    (labels ((get-or-create-next-drawable ()
               (when (>= index (length children))
                 (load-resources
                  (make-instance 'font-drawable
                                 :parent hud
                                 :width 150
                                 :height 100
                                 :x 0
                                 :y 0
                                 :color (slot-value hud 'text-color)
                                 :text "")
                  rendering-context)
                 (setf children (transform-children hud)))
               (elt children index))
             (update-drawable-to-match-stat (drawable stat-string)
               ;; doing the setf conses, which causes GC churn.
               ;; these values don't usually update, so check to see if the
               ;; value has changed before changing it.
               (unless (= (height drawable) (slot-value hud 'line-height))
                 (setf (height drawable) (slot-value hud 'line-height)))
               (unless (= (width drawable) (slot-value hud 'line-width))
                 (setf (width drawable) (slot-value hud 'line-width)))
               (unless (= (y drawable) (* index 10))
                 (setf (y drawable) (* index 10)))
               (unless (= (x drawable) (- (width hud) (width drawable) 40))
                 (setf (x drawable) (- (width hud) (width drawable) 40))) ; DONTCOMMIT: remove padding
               (unless (eq (text drawable) stat-string)
                 (setf (text drawable) stat-string)))
             (free-excess-drawables ()
               (loop :for i :from index :below (length children) :do
                    (release-resources (elt children i))
                    (setf (parent (elt children i)) nil))))
      (loop :for stat :across (slot-value hud 'game-stats) :do
           (loop :for stat-string :across (get-stats-strings stat) :do
                (update-drawable-to-match-stat
                 (get-or-create-next-drawable)
                 stat-string)
                (incf index)))
      (free-excess-drawables))))

(defmethod render :before ((hud stats-hud) update-percent camera rendering-context)
  (%render-stats-hud hud rendering-context))
