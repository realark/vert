(in-package :recurse.vert/unit-test)

(deftest camera-world-to-screen-conversion
  (let ((camera (make-instance 'camera
                               :target-max-offset 0
                               :screen-width 1000
                               :screen-height 1000
                               :pixels-per-unit 10))
        (obj (make-instance 'test-object)))
    (is (list 0 0)
        (multiple-value-list (recurse.vert::world-to-screen-cords obj camera))
        "obj in screen upper left"
        :test #'equalp)
    (setf (x obj) 99
          (y obj) 98)
    (is (list 990 980)
        (multiple-value-list (recurse.vert::world-to-screen-cords obj camera))
        "obj in screen lower right"
        :test #'equalp)
    (setf (x camera) 49
          (y camera) 48)
    (is (list 500 500)
        (multiple-value-list (recurse.vert::world-to-screen-cords obj camera))
        "obj in screen center"
        :test #'equalp)))

(deftest camera-target
  (let* ((object1 (make-instance 'test-object :x 10 :y 10))
         (object2 (make-instance 'test-object :x 100 :y 100))
         (camera (make-instance 'camera
                                :pixels-per-unit 1
                                :target-max-offset 0
                                :screen-width 1000
                                :screen-height 1000
                                :target object1)))
    ;; target correctly sets camera coords
    (is (x camera) -490
        "Camera initialized around target-x."
        :test #'float=)
    (is (y camera) -490
        "Camera initialized around target-y."
        :test #'float=)
    (setf (y object1) 20)
    (is (y camera) -480
        "Camera follows target-y"
        :test #'float=)
    (setf (x object1) 20)
    (is (x camera) -480
        "Camera follows target-x"
        :test #'float=)
    ;; should have no effect
    (setf (target camera) object1)
    (setf (x object1) 30)
    (is (x camera) -470
        "Setting the same target has no effect."
        :test #'float=)

    (setf (target camera) nil)
    (setf (x object1) 20)
    (is (x camera) -470
        "Target removed."
        :test #'float=)

    (setf (target camera) object2)
    (is (x camera) -400
        "Camera follows new target-x"
        :test #'float=)
    (is (y camera) -400
        "Camera follows new target-y"
        :test #'float=)

    (setf camera (make-instance 'camera
                                :pixels-per-unit 1
                                :target-max-offset 0
                                :screen-width 1000
                                :screen-height 1000))

    (setf (target camera) object2)
    (is (x camera) -400
        "New camera target-x"
        :test #'float=)
    (is (y camera) -400
        "New camera target-y"
        :test #'float=)
    ;; keep target and change camera dimensions
    (setf (recurse.vert::screen-width camera) 500)
    (is (x camera) -150
        "New screen-width updates camera x to keep target centered."
        :test #'float=)
    (setf (recurse.vert::screen-height camera) 500)
    (is (x camera) -150
        "Camera x unchanged after height change."
        :test #'float=)
    (is (y camera) -150
        "New screen-height updates camera y to keep target centered."
        :test #'float=)))

(deftest camera-zoom
  (let ((camera (make-instance 'camera
                               :target-max-offset 0
                               :pixels-per-unit 1
                               :zoom 1
                               :x 10 :y -10
                               :screen-width 800 :screen-height 600))
        (drawable (make-instance 'test-object
                                 :x 50 :y 80 :width 10 :height 20)))

    (render drawable 1.0 camera T)
    (with-accessors ((rect recurse.vert::sdl-rectangle)) drawable
      (is (sdl2:rect-width rect) 10)
      (is (sdl2:rect-height rect) 20)
      (is (sdl2:rect-x rect) 40)
      (is (sdl2:rect-y rect) 90))

    (setf (zoom camera) 2)
    (render drawable 1.0 camera T)
    (with-accessors ((rect recurse.vert::sdl-rectangle)) drawable
      (is (sdl2:rect-width rect) 20)
      (is (sdl2:rect-height rect) 40)
      (is (sdl2:rect-x rect) 80)
      (is (sdl2:rect-y rect) 180))

    (setf (zoom camera) .5)
    (render drawable 1.0 camera T)
    (with-accessors ((rect recurse.vert::sdl-rectangle)) drawable
      (is (sdl2:rect-width rect) 5)
      (is (sdl2:rect-height rect) 10)
      (is (sdl2:rect-x rect) 20)
      (is (sdl2:rect-y rect) 45))

    (setf (target camera) drawable)
    (render drawable 1.0 camera T)
    (with-accessors ((rect recurse.vert::sdl-rectangle)) drawable
      (is (sdl2:rect-width rect) 5)
      (is (sdl2:rect-height rect) 10)
      (is (sdl2:rect-x rect) 400)
      (is (sdl2:rect-y rect) 300))

    (setf (zoom camera) 2)
    (render drawable 1.0 camera T)
    (with-accessors ((rect recurse.vert::sdl-rectangle)) drawable
      (is (sdl2:rect-width rect) 20)
      (is (sdl2:rect-height rect) 40)
      (is (sdl2:rect-x rect) 400)
      (is (sdl2:rect-y rect) 300))))

(deftest camera-scale
  (let ((camera (make-instance 'camera
                               :pixels-per-unit 30 :zoom 1
                               :screen-width 800 :screen-height 600))
        (drawable (make-instance 'test-object :width 5 :height 2)))
    (is (width camera) (/ 800 30) :test #'float=)
    (is (height camera) (/ 600 30) :test #'float=)
    (with-accessors ((draw-width sdl2:rect-width)
                     (draw-height sdl2:rect-height))
        (recurse.vert::sdl-rectangle drawable)

      ;; test camera->screen coords
      (is (multiple-value-list (recurse.vert::world-to-screen-cords drawable camera))
          '(0 0)
          :test #'equalp)
      (move drawable 2 4)
      (is (multiple-value-list (recurse.vert::world-to-screen-cords drawable camera))
          '(60 120)
          :test #'equalp)
      (move drawable 100 100)
      (is (multiple-value-list (recurse.vert::world-to-screen-cords drawable camera))
          `(,(* 102 30) ,(* 104 30))
          :test #'equalp)
      (move drawable -104 -108)
      (is (multiple-value-list (recurse.vert::world-to-screen-cords drawable camera))
          '(-60 -120)
          :test #'equalp)

      ;; test camera->screen size
      (render drawable 1.0 camera T)
      (is draw-width 150)
      (is draw-height 60)
      (setf (zoom camera) 2)
      (is (width camera) (/ 800 (* 30 2)) :test #'float=)
      (is (height camera) (/ 600 (* 30 2)) :test #'float=)
      (render drawable 1.0 camera T)
      (is draw-width 300)
      (is draw-height 120)
      (setf (zoom camera) .5)
      (is (width camera) (/ 800 (* 30 .5)) :test #'float=)
      (is (height camera) (/ 600 (* 30 .5)) :test #'float=)
      (render drawable 1.0 camera T)
      (is draw-width 75)
      (is draw-height 30))))

(deftest camera-limit
  (let ((camera (make-instance 'camera
                               :pixels-per-unit 1
                               :target-max-offset 0
                               :zoom 1
                               :x 10 :y -10
                               :min-x -10 :min-y -50
                               :max-x 1100 :max-y 1000
                               :screen-width 800 :screen-height 600))
        (drawable (make-instance 'test-object :x 545 :y 545)))
    (move camera 2000 2000)
    (is (x camera) (- 1100 (/ (recurse.vert::screen-width camera) (zoom camera)))
        :test #'float=)
    (is (y camera) (- 1000 (/ (recurse.vert::screen-height camera) (zoom camera)))
        :test #'float=)
    (move camera -2000 -2000)
    (is (x camera) -10 :test #'float=)
    (is (y camera) -50 :test #'float=)

    (setf (zoom camera) 2)
    (move camera 2000 2000)
    (is (x camera) (- 1100 (/ (recurse.vert::screen-width camera) (zoom camera)))
        :test #'float=)
    (is (y camera) (- 1000 (/ (recurse.vert::screen-height camera) (zoom camera)))
        :test #'float=)
    (move camera -2000 -2000)
    (is (x camera) -10 :test #'float=)
    (is (y camera) -50 :test #'float=)

    (setf (zoom camera) 0.5)
    (move camera 2000 2000)
    (is (x camera) (- 1100 (/ (recurse.vert::screen-width camera) (zoom camera)))
        :test #'float=)
    (is (y camera) (- 1000 (/ (recurse.vert::screen-height camera) (zoom camera)))
        :test #'float=)
    (move camera -2000 -2000)
    (is (x camera) -10 :test #'float=)
    (is (y camera) -50 :test #'float=)

    (setf (target camera) drawable)
    (setf (zoom camera) 2)
    (multiple-value-bind (screen-x screen-y)
        (recurse.vert::world-to-screen-cords drawable camera)
      (is screen-x 400 :test #'float=)
      (is screen-y 300 :test #'float=))

    (setf (zoom camera) 0.5)
    (multiple-value-bind (screen-x screen-y)
        (recurse.vert::world-to-screen-cords drawable camera)
      (is screen-x 400 :test #'float=)
      (is screen-y 300 :test #'float=))

    ;; even though the drawable is the target, the camera should not move beyond the limits
    (move drawable 100 100)
    (multiple-value-bind (screen-x screen-y)
        (recurse.vert::world-to-screen-cords drawable camera)
      (is (ceiling screen-x) 473 :test #'float=)
      (is (ceiling screen-y) 345 :test #'float=))

    (move drawable -300 -300)
    (multiple-value-bind (screen-x screen-y)
        (recurse.vert::world-to-screen-cords drawable camera)
      (is (ceiling screen-x) 256 :test #'float=)
      (is (ceiling screen-y) 285 :test #'float=))

    (setf (zoom camera) 1.5)
    (multiple-value-bind (screen-x screen-y)
        (recurse.vert::world-to-screen-cords drawable camera)
      (is screen-x 400 :test #'float=)
      (is screen-y 300 :test #'float=))))

(deftest camera-offset
  (let* ((target (make-instance 'test-object
                                :x -1 :y -1
                                :width 2 :height 2))
         (max-camera-offset 100)
         (camera (make-instance 'camera
                                :target-max-offset max-camera-offset
                                :pixels-per-unit 1
                                :target target
                                :screen-width 1000
                                :screen-height 1000)))
    (setf (x target) -100)
    (setf (y target) 100)
    (is (x camera) -500
        "target-x within offset"
        :test #'float=)
    (is (y camera) -500
        "target-y within offset"
        :test #'float=)

    (setf (x target) -101)
    (setf (y target) 101)
    (is (x camera) -501
        "target-x exceeds offset. Camera moves."
        :test #'float=)
    (is (y camera) -499
        "target-y exceeds offset. Camera moves."
        :test #'float=)

    ;; reset target and recenter camera
    (setf (x target) -1)
    (setf (y target) -1)
    (setf (target camera) nil)
    (setf (target camera) target)
    ;; double zoom and run the same test
    (setf (zoom camera) 2)

    (setf (x target) -50)
    (setf (y target) 50)
    (is (x camera) -250
        "(2x) target-x within offset"
        :test #'float=)
    (is (y camera) -250
        "(2x) target-y within offset"
        :test #'float=)

    (setf (x target) -51)
    (setf (y target) 51)
    (is (x camera) -251
        "(2x) target-x exceeds offset. Camera moves."
        :test #'float=)
    (is (y camera) -249
        "(2x) target-y exceeds offset. Camera moves."
        :test #'float=)))
