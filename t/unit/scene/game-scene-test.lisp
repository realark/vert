(in-package :recurse.vert/unit-test)

(deftest scene-load-render-update-release
  "Game-scene should call update, render, load, and release on its game-objects."
  (let ((obj1 (make-instance 'test-object
                             :x 10 :y 10 :z 1))
        (obj2 (make-instance 'test-object
                             :x 90 :y 90 :z 0))
        (obj3 (make-instance 'test-object
                             :x 90 :y 90 :z 2))
        (scene (make-instance 'game-scene
                              :camera (make-instance 'camera :pixels-per-unit 1)
                              :width 100 :height 100)))
    (add-to-scene scene obj1)
    (add-to-scene scene obj1) ; shouldn't matter
    (add-to-scene scene obj2)
    (add-to-scene scene obj3)

    (render scene 0.0 nil nil)

    (is (method-invoke-count obj1 "load-resources") 1
        "game-object's resources loaded after scene add"
        :test #'=)

    (is (and (= 1
                (method-invoke-count obj1 "render")
                (method-invoke-count obj2 "render")
                (method-invoke-count obj3 "render"))
             (apply #'< (mapcar (lambda (obj)
                                  (method-invoke-id obj "render"))
                                (list obj2 obj1 obj3))))
        T
        "Objects rendered in z-layer order")

    (update scene 10 nil)
    (is (= 1
           (method-invoke-count obj1 "update")
           (method-invoke-count obj2 "update")
           (method-invoke-count obj3 "update"))
        T
        "scene updates game objects")

    (release-resources scene)
    (is (= 2
           (method-invoke-count obj1 "release-resources")
           (method-invoke-count obj2 "release-resources")
           (method-invoke-count obj3 "release-resources"))
        T
        "scene releases game-object resources")))

(deftest parallax-scrolling
  "Parallax backgrounds should scroll at different rates"
  (let ((bg-layer (make-instance 'scene-background
                                 :path-to-image (test-resource-path "rectangle.png")
                                 :width 100
                                 :height 100
                                 :horizontal-parallax .5
                                 :vertical-parallax .5))
        (fg-layer (make-instance 'scene-background
                                 :path-to-image (test-resource-path "rectangle.png")
                                 :width 100
                                 :height 100
                                 :horizontal-parallax 2.0
                                 :vertical-parallax 2.0))
        (player (make-instance 'test-object :width 10 :height 10))
        (camera (make-instance 'camera
                               :pixels-per-unit 1
                               :screen-width 1000
                               :screen-height 1000)))
    (flet ((screen-cords (object)
             (render object 1.0 camera T)
             (values (sdl2:rect-x (recurse.vert::sdl-rectangle object))
                     (sdl2:rect-y (recurse.vert::sdl-rectangle object)))))
      (incf (x camera) 10)
      (incf (y camera) 10)
      (is-values (screen-cords bg-layer) (list -5 -5))
      (is-values (screen-cords player) (list -10 -10))
      (is-values (screen-cords fg-layer) (list -20 -20)))))
