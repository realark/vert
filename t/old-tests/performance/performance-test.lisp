(in-package :recurse.vert/performance-test)


(deftest perf-test-render-static
  (run-performance-test :acceptable-average-fps 59
                        :acceptable-min-fps 40
                        :acceptable-heap-growth-percent 15
                        :rows 40 :cols 110
                        :start-x 50 :start-y 50
                        :run-time-seconds 30))

(deftest perf-test-broad-phase-collision
  (let ((acceleration (make-acceleration-vector-seconds :x 300)))
    (run-performance-test :rows 20 :cols 10
                          :object-creator
                          (lambda (&key row column)
                            (declare (ignore row column))
                            (make-instance 'test-object
                                           :on-update
                                           (lambda (object)
                                             (apply-vector
                                              object
                                              acceleration))
                                           :introspection-enabled nil
                                           :width 8
                                           :height 8
                                           :color recurse.vert::*black*))
                          :acceptable-average-fps 58
                          :acceptable-min-fps 40
                          :acceptable-heap-growth-percent 25
                          :row-space 40 :col-space 30
                          :start-x 50 :start-y 50
                          :run-time-seconds 30)))

(deftest perf-test-narrow-phase-collision
  (let ((acceleration (make-acceleration-vector-seconds :x 300)))
    (run-performance-test :rows 20 :cols 10
                          :object-creator
                          (lambda (&key row column)
                            (declare (ignore row column))
                            (make-instance 'test-object
                                           :on-update
                                           (lambda (object)
                                             (apply-vector
                                              object
                                              acceleration))
                                           :introspection-enabled nil
                                           :width 15.0
                                           :height 2.0
                                           :rotation (recurse.vert::deg->rad 45d0)
                                           :color recurse.vert::*black*))
                          :acceptable-average-fps 58
                          :acceptable-min-fps 40
                          :acceptable-heap-growth-percent 25
                          :row-space 20 :col-space 10
                          :start-x 50 :start-y 50
                          :run-time-seconds 30)))

(deftest perf-test-collision-resolution
  (let ((num-rows 20)
        (num-cols 9)
        (acceleration (make-acceleration-vector-seconds :x 300)))
    (run-performance-test :rows num-rows :cols num-cols
                          :object-creator
                          (lambda (&key row column)
                            (declare (ignore row))
                            (make-instance 'test-object
                                           :on-update
                                           (unless (= column (- num-cols 1))
                                             ;; final column won't move which will
                                             ;; cause collisions with all other objects
                                             (lambda (object)
                                               (apply-vector
                                                object
                                                acceleration)))
                                           :introspection-enabled nil
                                           :width 9.9
                                           :height 9.9
                                           :color recurse.vert::*black*))
                          :acceptable-average-fps 58
                          :acceptable-min-fps 40
                          :acceptable-heap-growth-percent 12
                          :start-x 50 :start-y 50
                          :run-time-seconds 30)))

(when nil ;; sample test. Will re-enable when sampling becomes important
  (deftest perf-test-audio-callbacks
    (let* ((samples-processed nil)
           (samples-valid T)
           (obj (make-instance 'test-music-queuer
                               :introspection-enabled nil
                               :on-update
                               (lambda (object)
                                 (declare (optimize (speed 3)))
                                 (loop with sample = (sb-concurrency:dequeue (slot-value object 'recurse.vert/test::amplitude-queue))
                                    while sample do
                                      (setf samples-processed T)
                                      (unless (= 0.0 (the (single-float -1.0 1.0) sample))
                                        ;; using the sample so it doesn't get optimized out by the compiler
                                        'do-nothing)
                                      (setf sample
                                            (sb-concurrency:dequeue (slot-value object 'recurse.vert/test::amplitude-queue)))))
                               :width 100 :height 100
                               :color recurse.vert::*blue*)))
      (run-performance-test :rows 1 :cols 1
                            :object-creator
                            (lambda (&key row column)
                              (declare (ignore row column))
                              (add-subscriber (audio-player *engine-manager*) obj music-advance sample-process)
                              (play-music (audio-player *engine-manager*) (test-resource-path "silence.wav") :num-plays -1)
                              obj)
                            :acceptable-average-fps 58
                            :acceptable-min-fps 58
                            :acceptable-heap-growth-percent 12
                            :start-x 50 :start-y 50
                            :run-time-seconds 30)
      (is samples-processed T "Samples were processed")
      (is samples-valid T "Samples were expected values."))))
