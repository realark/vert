(in-package :recurse.vert)

;;;; events

(prove:deftest test-event-sub
  'TODO
  (labels ((has-subscriber-p (pub sub event-name)
             "t of SUB is subscribed to PUB's EVENT-NAME"
             (let ((subs (gethash event-name
                                  (slot-value pub 'recurse.vert::event-subscribers))))
               (find sub subs)))
           (num-subs (pub event-name)
             "count of all of PUB's subs for EVENT-NAME"
             (let ((sub-arr (gethash event-name
                                     (slot-value pub 'recurse.vert::event-subscribers))))
               (loop :with count = 0
                  :for sub :across sub-arr :do
                    (when sub (incf count))
                    :finally (return count)))))
    (let ((pub (make-instance 'camera))
          ;; keep a strong ref to all subs to prevent GC finalizer cleanup
          (all-subs (list)))
      (loop :for i :from 0 :below 1000 :do
           (let ((sub (make-instance 'obb)))
             (push sub all-subs)
             (add-subscriber pub sub recurse.vert::object-moved)))
      (loop :for i :from 0 :below 20 :do
           (let ((sub (make-instance 'obb)))
             (push sub all-subs)
             (add-subscriber pub sub recurse.vert::camera-screen-resized)))
      (setf all-subs (nreverse all-subs))

      (prove:is (num-subs pub 'recurse.vert::object-moved) 1000
          "correct number of move subs")
      (prove:is (num-subs pub 'recurse.vert::camera-screen-resized) 20
          "correct number of screen resize subs")

      ;; first 1000 element should be in the moved subs, last 20 in the resize subs
      (loop :for i :from 0
         :with first-subs-moved-p = t
         :and second-subs-resized-p = t
         :for sub :in all-subs :do
           (if (< i 1000)
               (unless (or (numberp first-subs-moved-p)
                           (has-subscriber-p pub sub 'recurse.vert::object-moved))
                 (setf first-subs-moved-p i))
               (unless (or (numberp second-subs-resized-p)
                           (has-subscriber-p pub sub 'recurse.vert::camera-screen-resized))
                 (setf second-subs-resized-p i)))
         :finally
           (prove:is first-subs-moved-p t
               "First section of all-subs should be subbed to move")
           (prove:is second-subs-resized-p t
               "Second section of all-subs should be subbed to resize")))))

;;;; coord transforms

(prove:deftest test-obb-local-world-points
  (let* ((obj1 (make-instance 'obb
                              :width 10
                              :height 10
                              :x 10.0
                              :y 20.0
                              :z 2.0))
         (scaled-child (make-instance 'obb
                              :parent obj1
                              :width 5
                              :height 5
                              :scale-x 2.0
                              :scale-y 2.0
                              :x 1.0
                              :y 1.0
                              :z 1.0))
         (scaled-grandchild (make-instance 'obb
                              :parent scaled-child
                              :width 5
                              :height 5
                              :scale-x 2.0
                              :scale-y 2.0
                              :x 0.0
                              :y 0.0
                              :z 0.0))
         (rotated-child (make-instance 'obb
                                       :parent obj1
                                       :width 20
                                       :height 5
                                       :rotation (* tau 1/2)
                                       :x 10.0
                                       :y 20.0
                                       :z 0.0))
         (rotated-grandchild (make-instance 'obb
                                       :parent rotated-child
                                       :width 20
                                       :height 5
                                       :rotation (* tau 1/2)
                                       :x 0.0
                                       :y 0.0
                                       :z 0.0)))
    (prove:is (local-points obj1)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 10.0 0.0 0.0)
                (vector3 10.0 10.0 0.0)
                (vector3 0.0 10.0 0.0))
        "OBB local points correct"
        :test #'equalp)
    (prove:is (world-points obj1)
        (vector (vector3 10.0 20.0 2.0)
                (vector3 20.0 20.0 2.0)
                (vector3 20.0 30.0 2.0)
                (vector3 10.0 30.0 2.0))
        "OBB world points correct (direct child)"
        :test #'equalp)

    (prove:is (local-points scaled-child)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 5.0 0.0 0.0)
                (vector3 5.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "scaled child local points correct"
        :test #'equalp)
    (prove:is (world-points scaled-child)
        (vector (vector3 11.0 21.0 3.0)
                (vector3 21.0 21.0 3.0)
                (vector3 21.0 31.0 3.0)
                (vector3 11.0 31.0 3.0))
        "scaled child world points correct"
        :test #'equalp)

    (prove:is (local-points scaled-grandchild)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 5.0 0.0 0.0)
                (vector3 5.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "scaled grandchildchild local points correct"
        :test #'equalp)
    (prove:is (world-points scaled-grandchild)
        (vector (vector3 11.0 21.0 3.0)
                (vector3 31.0 21.0 3.0)
                (vector3 31.0 41.0 3.0)
                (vector3 11.0 41.0 3.0))
        "scaled grandchild world points correct"
        :test #'equalp)

    (prove:is (local-points rotated-child)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 20.0 0.0 0.0)
                (vector3 20.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "rotated child local points correct"
        :test #'equalp)
    (prove:is (world-points rotated-child)
        (vector (vector3 40.0 45.0 2.0)
                (vector3 20.0 45.0 2.0)
                (vector3 20.0 40.0 2.0)
                (vector3 40.0 40.0 2.0))
        "rotated child world points correct"
        :test #'equalp)

    (prove:is (local-points rotated-grandchild)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 20.0 0.0 0.0)
                (vector3 20.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "rotated grandchild local points correct"
        :test #'equalp)
    (let* ((world-points (world-points rotated-grandchild))
           (rounded-points (map 'vector (lambda (point)
                                          (vector3
                                           (float (round (x point)))
                                           (float (round (y point)))
                                           (float (round (z point)))))
                                world-points)))
      ;; rotated floats need to be rounded for meaningful comparison
      (prove:is rounded-points
          ;; grandchild is roated 180 degrees relative to child's 180 rotation
          ;; should end up back at a zero rotation
          (vector (vector3 20.0 40.0 2.0)
                  (vector3 40.0 40.0 2.0)
                  (vector3 40.0 45.0 2.0)
                  (vector3 20.0 45.0 2.0))
          "rotated child world points correct"
          :test #'equalp))))

(prove:deftest test-transform-points
  (let* ((basis1 (make-instance 'obb
                                :x 100
                                :y 100
                                :z 1))
         (basis2 (make-instance 'obb
                                :parent basis1
                                :rotation (* tau 1/2)
                                :scale-x 2.0
                                :scale-y 2.0
                                :width 10
                                :height 5
                                :x 1
                                :y 1
                                :z 1))
         (basis3 (make-instance 'obb
                                :parent nil
                                :rotation 0f0
                                :scale-x 2.0
                                :scale-y 3.0
                                :width 10
                                :height 5
                                :x 1.0
                                :y 1.0
                                :z 1.0)))
    (prove:is (transform-point (vector3 -100.0 -100.0 -1.0)
                         basis1)
        (vector3 0.0 0.0 0.0)
        :test #'equalp
        "transform point to world space")
    (prove:is (transform-point (vector3 1.0 1.0 1.0)
                         basis2)
        (vector3 119.0 109.0 3.0)
        "transform point from rotated and scaled child to world space"
        :test #'equalp)

    (prove:is (transform-point (vector3 1.0 1.0 1.0)
                         basis2
                         basis1)
        (vector3 19.0 9.0 2.0)
        "transform point from basis2 to basis1"
        :test #'equalp)

    (prove:is (transform-point (vector3 1.0 1.0 1.0)
                         basis3)
        (vector3 3.0 4.0 2.0)
        "transform point with scaling"
        :test #'equalp)

    (multiple-value-bind (x y z w h) (world-dimensions basis3)
      (prove:is x 1.0 "x point scaled by 2" :test #'equalp)
      (prove:is y 1.0 "y point scaled by 3" :test #'equalp)
      (prove:is z 1.0 "z not scaled" :test #'equalp)
      (prove:is w 20 "width scaled by 2" :test #'equalp)
      (prove:is h 15 "height scaled by 3" :test #'equalp))))

(prove:deftest test-vector-distance
  (let ((p1 (vector3 -10f0 -17f0 -2f0))
        (p2 (vector3 13f0 0f0 3f0)))
    (prove:is (distance-between p1 p2) 29.03446
        "Precise distance between points."
        :test #'float=)))

;;;; collisions

(defclass %phantom-obb (phantom obb) ())

(prove:deftest test-collision-phantoms
  (let ((phantom1 (make-instance '%phantom-obb
                                 :x 10
                                 :y 10
                                 :width 10
                                 :height 10))
        (phantom2 (make-instance '%phantom-obb
                                 :x 15
                                 :y 15
                                 :width 10
                                 :height 10))
        (box1 (make-instance 'obb
                             :x 9
                             :y 9
                             :width 7
                             :height 7))
        (triangle (make-instance 'convex-polygon
                                 :x 16
                                 :y 16
                                 :local-points
                                 (vector(vector3 0.0 0.0 0.0)
                                        (vector3 1.0 0.0 0.0)
                                        (vector3 0.5 1.0 0.0)))))
    (prove:is (collidep phantom1 box1)
        t
        "phantoms collide with normal objects")
    (prove:is (collidep box1 phantom1)
        nil
        "normal objects do not collide with phantoms")
    (prove:is (collidep phantom1 phantom2)
        nil
        "phantoms do not collide with each other.")
    (prove:is (collidep phantom2 triangle)
        nil
        "phantoms collide with polygons")
    (prove:is (collidep triangle phantom2)
        nil
        "polygons do not collide with phantoms")))
