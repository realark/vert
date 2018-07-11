(in-package :recurse.vert/test)

(defclass test-scene (game-scene physics-context-2d test-introspector)
  ;; command running
  ((until-timestamp :initform 0)
   (command-stack :initarg :command-stack
                  :initform '()
                  :documentation "Stack of command sequences to process")
   (command-index :initform 0)
   (next-update-task :initform nil
                     :documentation "function to be invoked next update frame")
   ;; framerate measuring
   (next-frame-sample :initform nil)
   (framerate-sample-window-ms :initform 100) ; measure framerate every 100ms
   (num-frames :initform 0)
   (framerate-samples :initform '())
   ;; heap size measuring
   (initial-heap-size
    :accessor initial-heap-size
    :initform nil)
   (final-heap-size
    :accessor final-heap-size
    :initform nil))
  (:documentation "Scene for testing"))

(defun reset-framerate-measurements (test-scene)
  (with-slots (next-frame-sample
               num-frames
               framerate-samples)
      test-scene
    (setf next-frame-sample nil
          num-frames 0
          framerate-samples '())))

(defun start-framerate-measuring (test-scene &optional (fresh T))
  (when fresh
    (reset-framerate-measurements test-scene))
  (setf (slot-value test-scene 'num-frames) 0
        (slot-value test-scene 'next-frame-sample)
        (+ (slot-value test-scene 'framerate-sample-window-ms) (ticks))))

(defun stop-framerate-measuring (test-scene)
  (setf (slot-value test-scene 'num-frames) 0
        (slot-value test-scene 'next-frame-sample) nil))

(defun average-framerate (test-scene)
  (with-slots (framerate-samples) test-scene
    (if framerate-samples
        (/ (apply #'+ framerate-samples)
           (length framerate-samples))
        -1)))

(defun min-framerate (test-scene)
  (with-slots (framerate-samples) test-scene
    (if framerate-samples
        (first (sort (copy-list framerate-samples) #'<))
        -1)))

(defun %dynamic-heap-size-bytes ()
  (let ((stream (make-string-output-stream)))
    (let ((*standard-output* stream))
      (room nil))
    (let* ((room-string (get-output-stream-string stream))
           (total-heap-line (first (split-sequence:split-sequence  #\newline room-string))))
      (parse-integer (remove-if-not #'digit-char-p total-heap-line)))))

(defun start-heap-measurements (test-scene)
  (with-slots (initial-heap-size final-heap-size) test-scene
    (setf initial-heap-size (%dynamic-heap-size-bytes)
          final-heap-size nil)))

(defun stop-heap-measurements (test-scene)
  (with-slots (final-heap-size) test-scene
    (setf final-heap-size (%dynamic-heap-size-bytes))))

(defun heap-diff-bytes (test-scene)
  (with-slots (initial-heap-size final-heap-size) test-scene
    (when (and initial-heap-size final-heap-size)
      (- final-heap-size initial-heap-size))))

(defun heap-diff-percent (test-scene)
  (with-slots (initial-heap-size final-heap-size) test-scene
    (when (and initial-heap-size final-heap-size)
      (* 100 (/ (heap-diff-bytes test-scene) initial-heap-size)))))

(defmethod update ((test-scene test-scene) delta-t-ms world-context)
  (notice-method-invoked test-scene "update")
  (with-slots (command-stack command-index) test-scene
    (when command-stack
      (funcall (elt (first command-stack) command-index) test-scene)
      (when (>= command-index (length (first command-stack)))
        (pop command-stack)
        (setf command-index 0))))
  (call-next-method test-scene delta-t-ms world-context))

(defmethod render ((test-scene test-scene) (update-percent real) (camera camera) rendering-context)
  (notice-method-invoked test-scene "render")
  (with-slots (next-frame-sample
               num-frames
               framerate-sample-window-ms
               framerate-samples)
      test-scene
    (let ((now (ticks)))
      (when (and next-frame-sample
                 (>= now next-frame-sample))
        (push (* num-frames (/ 1000 framerate-sample-window-ms)) framerate-samples)
        (setf num-frames 0)
        (setf next-frame-sample (+ now framerate-sample-window-ms))))
    (incf num-frames)
    (call-next-method test-scene update-percent camera rendering-context)))

(defun %wait-ms-command (scene ms-to-wait)
  (with-slots (command-index until-timestamp) scene
    (when (<= until-timestamp 0)
      (setf until-timestamp (+ (ticks) ms-to-wait)))
    (when (>= (ticks) until-timestamp)
      (setf until-timestamp 0)
      (incf command-index))))

(defun %run-scene-command (scene command-name &rest command-args)
  (ecase command-name
    (wait-ms (assert (and (= 1 (length command-args))
                          (numberp (first command-args))))
             (%wait-ms-command scene (first command-args)))))

(defmacro make-test-scene (scene-binding (&rest test-scene-options) &body body)
  (alexandria:with-gensyms (command-stack)
    `(make-instance 'test-scene
                    ,@test-scene-options
                    :command-stack
                    (let ((,command-stack nil))
                      ;; command-stack = stack of command-lists
                      ;;   when the stack is empty the scene will quit the game
                      ;; command-list  = list of commands to run
                      ;;   when command-list finishes the stack is popped
                      ,@(loop with stack-builder = '()
                           and command-list = '()
                           for command in body do
                             (push (case (intern (symbol-name (first command))
                                                 (symbol-package 'update-frame))
                                     (update-frame `(lambda (,scene-binding)
                                                      (incf (slot-value ,scene-binding 'command-index))
                                                      ,@(rest command)))
                                     (otherwise `(lambda (,scene-binding)
                                                   (%run-scene-command
                                                    ,scene-binding
                                                    ',(intern (symbol-name (first command))
                                                              (symbol-package 'update-frame))
                                                    ,@(rest command)))))
                                   command-list)
                           finally
                             (when command-list
                               (push `(push (list ,@(nreverse command-list))
                                            ,command-stack)
                                     stack-builder))
                             (return (nreverse stack-builder)))
                      (nreverse ,command-stack)))))


(defun run-performance-test (&key (object-creator (lambda (&key row column)
                                                    (declare (ignore row column))
                                                    (make-instance 'test-object
                                                                   :introspection-enabled nil
                                                                   :width 9
                                                                   :height 9
                                                                   :color recurse.vert::*black*)))
                                                        (run-time-seconds 30)
                                                        (start-x 1) (start-y 1)
                                                        (rows 10) (cols 20)
                                                        (row-space 10)
                                                        (col-space 10)
                                                        (acceptable-heap-growth-percent nil)
                                                        (acceptable-average-fps nil)
                                                        (acceptable-min-fps nil)
                                                        (profile-entire-test nil)
                                                        (profile-game-loop nil))
  (when (and profile-entire-test profile-game-loop)
    (setf profile-game-loop nil))
  (sb-ext:gc :full T)

  (when profile-entire-test
    (swank:profile-reset))
  (let ((scene (make-test-scene scene (:width 1920 :height 1015
                                              :introspection-enabled nil
                                              :camera (make-instance 'camera :pixels-per-unit 1))
                 (update-frame
                  (when profile-game-loop
                    (swank:profile-reset))
                  (start-heap-measurements scene)
                  (start-framerate-measuring scene))
                 (wait-ms (* 1000 run-time-seconds))
                 (update-frame
                  (stop-framerate-measuring scene)
                  (stop-heap-measurements scene)
                  (when profile-game-loop
                    (swank:profile-report))
                  (quit)))))
    (main (lambda ()
            (loop for r from 0 below rows do
                 (loop for c from 0 below cols do
                      (let ((obj (funcall object-creator :row r :column c)))
                        (setf (x obj) (+ start-x (* col-space c))
                              (y obj) (+ start-y (* row-space r)))
                        (add-to-scene scene obj))))
            (resize-window (application-window *engine-manager*) 1920 1015)
            (setf (clear-color *engine-manager*) recurse.vert::*yelllow*)
            scene))
    (when profile-entire-test
      (swank:profile-report))
    (when acceptable-average-fps
      (prove:is (average-framerate scene) acceptable-average-fps
                (format nil "Average Framerate: ~Afps" (floor (average-framerate scene)))
                :test #'>=))
    (when acceptable-min-fps
      (prove:is (min-framerate scene) acceptable-min-fps
                (format nil "Min Framerate: ~Afps" (floor (min-framerate scene)))
                :test #'>=))
    (when acceptable-heap-growth-percent
      (prove:is (heap-diff-percent scene) acceptable-heap-growth-percent
                (format nil "Heap grew ~:d bytes (~,2f%)"
                        (heap-diff-bytes scene)
                        (heap-diff-percent scene))
                :test #'<=))))
