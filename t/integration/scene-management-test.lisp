(in-package :recurse.vert/integration-test)

(deftest scene-manager
  (let ((scene1-invoked nil)
        (scene2-invoked nil))
    (main (lambda ()
            (let* ((input (make-instance 'input-device :input-name "test-keyboard"))
                   (obj1 (make-instance 'test-object))
                   (obj2 (make-instance 'test-object))
                   (scene2 (make-test-scene scene (:width 10 :height 10)
                             (update-frame
                              (pass "Switched to Scene2")
                              (setf scene2-invoked T))
                             (update-frame
                              (is (method-invoke-count obj2 "while-active-move-left")
                                  1
                                  "engine-manager hooks input to active scene")
                              (is (method-invoke-count obj1 "while-active-move-left")
                                  1
                                  "engine-manager does not hook input to deactivated scene")
                              (quit))))
                   (scene1 (make-test-scene scene (:width 10 :height 10)
                             (update-frame
                              (setf scene1-invoked T))
                             (update-frame
                              (is (method-invoke-count obj1 "while-active-move-left")
                                  1
                                  "engine-manager hooks input to active scene")
                              (change-scene *engine-manager* scene2))
                             (update-frame
                              (fail "scene2 should load before this update frame is invoked.")
                              (quit)))))
              (register-input-device (input-manager *engine-manager*) input)
              (activate-input input :scancode-left)
              (setf (active-input-device obj1) (device-id input))
              (setf (active-input-device obj2) (device-id input))
              (add-to-scene scene1 obj1)
              (add-to-scene scene2 obj2)
              scene1)))
    (is scene1-invoked T "Engine Manager Updates Scene1")
    (is scene2-invoked T "Engine Manager Updates Scene2")))
