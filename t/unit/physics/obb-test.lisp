(in-package :recurse.vert/unit-test)

(deftest obb-local-world-points
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
    (is (local-points obj1)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 10.0 0.0 0.0)
                (vector3 10.0 10.0 0.0)
                (vector3 0.0 10.0 0.0))
        "OBB local points correct"
        :test #'equalp)
    (is (world-points obj1)
        (vector (vector3 10.0 20.0 2.0)
                (vector3 20.0 20.0 2.0)
                (vector3 20.0 30.0 2.0)
                (vector3 10.0 30.0 2.0))
        "OBB world points correct (direct child)"
        :test #'equalp)

    (is (local-points scaled-child)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 5.0 0.0 0.0)
                (vector3 5.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "scaled child local points correct"
        :test #'equalp)
    (is (world-points scaled-child)
        (vector (vector3 11.0 21.0 3.0)
                (vector3 21.0 21.0 3.0)
                (vector3 21.0 31.0 3.0)
                (vector3 11.0 31.0 3.0))
        "scaled child world points correct"
        :test #'equalp)

    (is (local-points scaled-grandchild)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 5.0 0.0 0.0)
                (vector3 5.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "scaled grandchildchild local points correct"
        :test #'equalp)
    (is (world-points scaled-grandchild)
        (vector (vector3 11.0 21.0 3.0)
                (vector3 31.0 21.0 3.0)
                (vector3 31.0 41.0 3.0)
                (vector3 11.0 41.0 3.0))
        "scaled grandchild world points correct"
        :test #'equalp)

    (is (local-points rotated-child)
        (vector (vector3 0.0 0.0 0.0)
                (vector3 20.0 0.0 0.0)
                (vector3 20.0 5.0 0.0)
                (vector3 0.0 5.0 0.0))
        "rotated child local points correct"
        :test #'equalp)
    (is (world-points rotated-child)
        (vector (vector3 40.0 45.0 2.0)
                (vector3 20.0 45.0 2.0)
                (vector3 20.0 40.0 2.0)
                (vector3 40.0 40.0 2.0))
        "rotated child world points correct"
        :test #'equalp)

    (is (local-points rotated-grandchild)
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
      (is rounded-points
          ;; grandchild is roated 180 degrees relative to child's 180 rotation
          ;; should end up back at a zero rotation
          (vector (vector3 20.0 40.0 2.0)
                  (vector3 40.0 40.0 2.0)
                  (vector3 40.0 45.0 2.0)
                  (vector3 20.0 45.0 2.0))
          "rotated child world points correct"
          :test #'equalp))))

(deftest transform-points
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
                                :z 1)))
    (is (transform-point (vector3 -100.0 -100.0 -1.0)
                         basis1)
        (vector3 0.0 0.0 0.0)
        :test #'equalp
        "transform point to world space")
    (is (transform-point (vector3 1.0 1.0 1.0)
                         basis2)
        (vector3 119.0 109.0 3.0)
        "transform point from rotated and scaled child to world space"
        :test #'equalp)

    (is (transform-point (vector3 1.0 1.0 1.0)
                         basis2
                         basis1)
        (vector3 19.0 9.0 2.0)
        "transform point from basis2 to basis1"
        :test #'equalp)))
