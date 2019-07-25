(in-package :recurse.vert/integration-test)

(deftest application-window
  (main (lambda ()
          (let ((window (application-window *engine-manager*)))
            (resize-window window 100 300)
            (is-values
             (window-size-pixels window)
             (list 100 300)
             "application window resized")
            (toggle-fullscreen window)
            (is-values
             (window-size-pixels window)
             (list 100 300)
             "application window in fullscreen mode"
             :test (lambda (got expected)
                     (every (lambda (expected got)
                              (> got expected))
                            expected
                            got)))
            (toggle-fullscreen window)
            (is-values
             (window-size-pixels window)
             (list 100 300)
             "application window quit fullscreen mode")
            (quit)
            (make-instance 'test-scene :width 10 :height 10)))))

(deftest misc-engine-features
  (main (lambda ()
          (is (main (lambda ()
                      (fail "should not run")))
              nil
              "can't nest game windows")
          (progn ; engine cache
            (register-cache (memory-manager *engine-manager*)
                            "mycache"
                            (make-instance 'evict-oldest-cache))
            (setf (getcache 'foo (get-registered-cache (memory-manager *engine-manager*) "mycache"))
                  'bar)
            (is (getcache 'foo (get-registered-cache (memory-manager *engine-manager*) "mycache"))
                'bar
                "memory manager manages cache")
            (deregister-cache (memory-manager *engine-manager*) "mycache"))
          (quit)
          (make-instance 'test-scene :width 10 :height 10))))

(deftest texture-cache
  (let ((tex-cache nil))
    (main (lambda ()
            (let* ((texture-cache recurse.vert::*resource-cache*)
                   (obj1 (make-instance 'test-object
                                        :color recurse.vert::*blue*
                                        :x 1 :y 1
                                        :width 1 :height 1))
                   (obj2 (make-instance 'test-object
                                        :color recurse.vert::*red*
                                        :width 1 :height 2
                                        :x 10 :y 10))
                   (obj3 (make-instance 'test-circle
                                        :color recurse.vert::*green*
                                        :width 2
                                        :height 2
                                        :x 15 :y 15))
                   (scene (make-test-scene scene (:width 500 :height 500)
                            (update-frame
                             (is (not (null (getcache (test-resource-path "rectangle.png") texture-cache)))
                                 T
                                 "rectangle texture cached")
                             (is (not (null (getcache (test-resource-path "circle.png") texture-cache)))
                                 T
                                 "circle texture cached"))
                            (update-frame (quit)))))
              (is (not (null texture-cache)) T "texture cache initialized")
              (setf tex-cache texture-cache)
              (add-to-scene scene obj1)
              (add-to-scene scene obj2)
              (add-to-scene scene obj3)
              scene)))
    (is (null (getcache (test-resource-path "circle.png") tex-cache))
        T
        "circle texture cleared from cache after game exit")
    (is (null (getcache (test-resource-path "rectangle.png") tex-cache))
        T
        "rectangle texture cleared from cache after game exit")))
