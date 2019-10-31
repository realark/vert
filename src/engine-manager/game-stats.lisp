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

@export
(defgeneric get-stats-strings (game-stats)
  (:documentation "Return an array of strings for all stats and reset GAME-STATS timers if appropriate. Each entry will be rendered per line of dev-mode info.")
  (:method ((game-stats game-stats)) (make-array 0 :element-type 'string)))

;;;; stats which measure some type of framerate
(defclass frame-timer (game-stats)
  ((prefix :initarg :prefix :initform "")
   (stats-array :initform (make-array 3 :element-type 'string :initial-element ""))
   (timer-start-timestamp :initform (ticks-nanos)
                          :documentation "timestamp of when this frame timer began to measure frames.")
   (frame-start-timestamp :initform nil
                          :documentation "timestamp of when the last frame being measure began.")
   (sum-frames :initform 0
               :documentation "Total frame time (nanoseconds)")
   (num-frames :initform 0
               :documentation "Number of frames seen")
   (longest-frames :initform (make-array 10 :element-type 'fixnum :adjustable nil :initial-element -1)
                   :documentation "Longest 10 frames observed in the current measurement period. 0 = largest frame seen so far.")))

(defun frame-timer-start-timer (frame-timer)
  (declare (optimize (speed 3)))
  (with-slots (frame-start-timestamp) frame-timer
    (setf frame-start-timestamp (ticks-nanos))))

(defun frame-timer-stop-timer (frame-timer)
  (declare (optimize (speed 3)))
  (with-slots (frame-start-timestamp sum-frames num-frames) frame-timer
    (when frame-start-timestamp
      (locally (declare (fixnum frame-start-timestamp num-frames sum-frames))
        (labels ((insert-at-index (array index element)
                   "insert ELEMENT into ARRAY at INDEX. All elements after INDEX will be downshifted and the final element removed from ARRAY."
                   (loop :with current-element = element
                      :while (< index (length array))
                      :do
                        (let ((tmp (elt array index)))
                          (setf (elt array index) current-element
                                current-element tmp)
                          (incf index))))
                 (add-longest-frame-time (frame-timer current-frame-time)
                   (declare (frame-timer frame-timer)
                            (fixnum current-frame-time))
                   (with-slots (longest-frames) frame-timer
                     (declare ((simple-array fixnum) longest-frames))
                     (when (> current-frame-time (elt longest-frames (- (length longest-frames) 1)))
                       (loop :with insertion-index = (- (length longest-frames) 1)
                          :while (and (> insertion-index 0)
                                      (> current-frame-time (elt longest-frames (- (length longest-frames) 1))))
                          :do (decf insertion-index)
                          :finally
                            (insert-at-index longest-frames insertion-index current-frame-time))))))
          (declare (inline insert-at-index add-longest-frame-time))
          (let ((frame-time (- (the fixnum (ticks-nanos)) frame-start-timestamp)))
            (declare (fixnum frame-time))
            (incf sum-frames frame-time)
            (incf num-frames)
            (add-longest-frame-time frame-timer frame-time)))))))

(defun frame-timer-stats (frame-timer)
  "Get stats for the time period covering the last reset for FRAME-TIMER to now. (values avg-frame-time-ns frames-per-second)"
  (declare (optimize (speed 3)))
  (with-slots (timer-start-timestamp frame-start-timestamp sum-frames num-frames longest-frames) frame-timer
    (declare (fixnum timer-start-timestamp sum-frames num-frames))
    (if (= 0 num-frames)
        (values 0 0.0 0.0)
        (let* ((delta-t-ns (- (the fixnum (ticks-nanos)) timer-start-timestamp))
               (avg-frame-time-ns (/ sum-frames num-frames))
               (fps (/ num-frames (/ (the fixnum delta-t-ns) #.(expt 10.0 9))))
               (longest-average (loop :with sum-frames = 0
                                   :for i :from 0 :below (length longest-frames) :do
                                     (incf sum-frames (elt longest-frames i))
                                   :finally (return (float (/ sum-frames (length longest-frames)))))))
          (values avg-frame-time-ns fps longest-average)))))

(defun frame-timer-reset (frame-timer)
  (labels ((reset-longest-frames (frame-timer)
             (with-slots (longest-frames) frame-timer
               (declare ((simple-array fixnum) longest-frames))
               (loop :for i :from 0 :below (length longest-frames) :do
                    (setf (elt longest-frames i) -1)))))
    (declare (inline reset-longest-frames))
    (with-slots (timer-start-timestamp frame-start-timestamp sum-frames num-frames) frame-timer
      (setf timer-start-timestamp (ticks-nanos)
            frame-start-timestamp nil
            sum-frames 0
            num-frames 0)
      (reset-longest-frames frame-timer)
      frame-timer)))

(defmethod get-stats-strings ((frame-timer frame-timer))
  (multiple-value-bind (avg-frame-time-ns fps longest-average-ns)
      (frame-timer-stats frame-timer)
    (prog1
        (with-slots (stats-array prefix longest-frames) frame-timer
          (declare ((simple-array fixnum) longest-frames))
          (setf (elt stats-array 0) (format nil "~Afps: ~A" prefix (truncate-float fps +default-stats-decimal-places+))
                (elt stats-array 1) (format nil "~Aavg-ms: ~A" prefix (truncate-float (/ avg-frame-time-ns #.(expt 10.0 6))
                                                                                      +default-stats-decimal-places+))
                (elt stats-array 2) (format nil "~Awrst-avg-ms: ~A" prefix (truncate-float (/ longest-average-ns #.(expt 10.0 6)) +default-stats-decimal-places+)))
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
                         :initform 2000)
   (stats-array :initform (make-array 10 :element-type 'string :initial-element ""))
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
  ((game-stats :initarg :game-stats :initform (error ":game-stats required"))
   (stats-font-size :initform 4)
   (text-color :initform (make-color-rgba :r 255)))
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
                                 :font-size (slot-value hud 'stats-font-size)
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
               (unless (eq (text drawable) stat-string)
                 (setf (text drawable) stat-string))
               (unless (= (y drawable) (* index 10))
                 (setf (y drawable) (* index 10)))
               (unless (= (x drawable) (- (width hud) (width drawable)))
                 (setf (x drawable) (- (width hud) (width drawable)))))
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
