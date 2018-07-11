(in-package :recurse.vert/unit-test)

(deftest back-and-forth-test
  (let* ((ai (make-instance 'back-and-forth :width 50 :height 50
                            :agent-acceleration (make-acceleration-vector-seconds :x 600 :y -200))))
    (update ai 17 T) ;; agent move in initial direction
    (is (< (acceleration-y ai) 0) T "Agent accel down.")
    (is (> (acceleration-x ai) 0) T "Agent accel right.")
    (setf (acceleration-x ai) 0)
    (setf (acceleration-y ai) 0)

    (collision ai (make-instance 'obb :width 100 :height 100 :x 50 :y 0))
    (update ai 17 T)
    (is (acceleration-y ai) 0 "Agent accel up." :test #'>)
    (is (acceleration-x ai) 0 "Agent accel left." :test #'<)))

(deftest jumper-test
  (let ((jumper (make-instance 'jumper
                               :jump-velocity 100 :jump-height 80
                               :width 30 :height 50))
        (ground (make-instance 'obb :width 1000 :height 10 :x -500 :y (+ 50.0 recurse.vert::*collision-precision*)))
        (timestep-ms 10)
        (world (make-instance 'platformer-game-scene :width 100 :height 100))
        (max-distance-jumped 0))
    (add-to-scene world jumper)
    (add-to-scene world ground)
    (let ((initial-y (y jumper))
          (initial-v (velocity-y jumper))
          (initial-a (acceleration-y jumper)))
      (jump jumper)
      (update jumper timestep-ms world)
      (prove:is (and (= (y jumper) initial-y)
                     (= (velocity-y jumper) initial-v)
                     (= (acceleration-y jumper) initial-a))
                T "Can't jump when off ground"))

    (collision jumper ground)
    (prove:is (and (eq (first (objects-touching jumper)) ground)
                   (null (is-jumping jumper))
                   (null (is-falling jumper)))
              T
              "jumper on ground")

    ;; regular jump
    (progn
      (jump jumper)
      (update world timestep-ms nil)
      (when (jump-start-ts jumper)
        (setf (jump-start-ts jumper) (- (jump-start-ts jumper) timestep-ms)))
      (prove:is (length (objects-touching jumper)) 0 "jumper off ground")
      (setf max-distance-jumped (abs (y jumper)))
      (loop for i from 0 below 200 by 1 do
           (when (= 0 (length (objects-touching jumper)))
             (jump jumper))
           (update world timestep-ms nil)
           (when (jump-start-ts jumper)
             (setf (jump-start-ts jumper) (- (jump-start-ts jumper) timestep-ms)))
           (let ((jumped (abs (y jumper))))
             (when (> jumped max-distance-jumped)
               (setf max-distance-jumped jumped))))
      (prove:is (and (eq (first (objects-touching jumper)) ground)
                     (null (is-jumping jumper))
                     (null (is-falling jumper)))
                T
                "jumper on ground")
      (prove:is (< (abs (- max-distance-jumped 80)) 3) T
                (format nil "Expected max distance (~A) to be approximately 80" max-distance-jumped)))

    ;; double jump height
    (progn
      (setf (slot-value jumper 'recurse.vert::jump-height) 160)
      (jump jumper)
      (update world timestep-ms nil)
      (when (jump-start-ts jumper)
        (setf (jump-start-ts jumper) (- (jump-start-ts jumper) timestep-ms)))
      (prove:is (length (objects-touching jumper)) 0 "jumper off ground")
      (setf max-distance-jumped (abs (y jumper)))
      (loop for i from 0 below 400 by 1
         do
           (when (= 0 (length (objects-touching jumper)))
             (jump jumper))
           (update world timestep-ms nil)
           (when (jump-start-ts jumper)
             (setf (jump-start-ts jumper) (- (jump-start-ts jumper) timestep-ms)))
           (let ((jumped (abs (y jumper))))
             (when (> jumped max-distance-jumped)
               (setf max-distance-jumped jumped))))
      (prove:is (first (objects-touching jumper)) ground (format nil "jumper (~A) on ground" jumper))
      (prove:is (< (abs (- max-distance-jumped 160)) 5) T
                (format nil "Expected max distance (~A) to be approximately 160" max-distance-jumped))
      (setf (slot-value jumper 'recurse.vert::jump-height) 80))

    ;; double jump velocity
    (progn
      (setf (slot-value jumper 'recurse.vert::jump-velocity) 200)
      (jump jumper)
      (update world timestep-ms nil)
      (when (jump-start-ts jumper)
        (setf (jump-start-ts jumper) (- (jump-start-ts jumper) timestep-ms)))
      (prove:is (length (objects-touching jumper)) 0 "jumper off ground")
      (setf max-distance-jumped (abs (y jumper)))
      (loop for i from 0 below 200 by 1
         do
           (when (= 0 (length (objects-touching jumper)))
             (jump jumper))
           (update world timestep-ms nil)
           (when (jump-start-ts jumper)
             (setf (jump-start-ts jumper) (- (jump-start-ts jumper) timestep-ms)))
           (let ((jumped (abs (y jumper))))
             (when (> jumped max-distance-jumped)
               (setf max-distance-jumped jumped))))
      (prove:is (first (objects-touching jumper)) ground (format nil "jumper (~A) on ground" jumper))
      ;; FIXME make this more precise
      (prove:is (< (abs (- max-distance-jumped 80)) 10) T
                (format nil "Expected max distance (~A) to be approximately 80"
                        (float max-distance-jumped)))
      (setf (slot-value jumper 'recurse.vert::jump-velocity) 100))))

(deftest direction-tracker-test
  (let ((face (make-instance 'direction-tracker :width 1 :height 1
                             :facing '(:NORTH :EAST))))
    (push-direction face :NORTH)
    (push-direction face :EAST)
    (prove:is (facing face)
              '(:NORTH :EAST)
              :test #'equalp)
    (push-direction face :WEST)
    (prove:is (facing face)
              '(:NORTH :WEST)
              :test #'equalp)
    (push-direction face :SOUTH)
    (prove:is (facing face)
              '(:SOUTH :WEST)
              :test #'equalp)

    (push-direction face :WEAST)
    (prove:is (facing face)
              '(:SOUTH :WEST)
              "Patrick, there's no such thing as weast."
              :test #'equalp)))
