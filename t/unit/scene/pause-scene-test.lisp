(in-package :recurse.vert/unit-test)

(deftest pause-scene-and-step
  (flet ((can-update (pause-scene)
           "T if pause-scene's underlying scene can update"
           (let ((obj (make-instance 'test-object)))
             (with-slots (scene) pause-scene
               (setf (velocity-x obj) 100)
               ;;add
               (add-to-scene scene obj)
               ;;update
               (update pause-scene 100 nil)
               ;;remove
               (remove-from-scene scene obj)
               ;;test
               (>= (method-invoke-count obj "update") 1)))))
    (let* ((scene (make-instance 'game-scene :width 100 :height 100))
           (pause-scene (make-instance 'pause-scene :scene scene)))
      (is (can-update pause-scene) nil "scene paused.")

      (step-scene pause-scene :num-steps 1 :time-between-frames-ms 0)
      (is (can-update pause-scene) T "scene stepping")
      (is (can-update pause-scene) nil "scene pauses after stepping")

      (step-scene pause-scene :num-steps 3 :time-between-frames-ms 0)
      (is (can-update pause-scene) T "scene stepping 3")
      (is (can-update pause-scene) T "scene stepping 2")
      (is (can-update pause-scene) T "scene stepping 1")
      (is (can-update pause-scene) nil "scene paused"))))
