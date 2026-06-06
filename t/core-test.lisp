(in-package :recurse.vert)

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
    #+nil
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

    #+nil
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

    #+nil
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

    #+nil
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

    #+nil
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

;;;; matrix math

(prove:deftest test-matrix-multiply-into
  ;; matrix*-into must produce the same result as the allocating matrix*.
  (let* ((a (matrix 1f0 2f0 3f0 4f0    5f0 6f0 7f0 8f0
                    9f0 10f0 11f0 12f0 13f0 14f0 15f0 16f0))
         (b (matrix 16f0 15f0 14f0 13f0 12f0 11f0 10f0 9f0
                    8f0 7f0 6f0 5f0     4f0 3f0 2f0 1f0))
         (expected (matrix* a b))
         (result (identity-matrix)))
    (prove:is (matrix*-into result a b) expected
              "matrix*-into matches matrix*"
              :test #'equalp)
    ;; identity * m == m
    (prove:is (matrix*-into (identity-matrix) (identity-matrix) a) a
              "identity * a == a"
              :test #'equalp)))

(defun %bytes-consed (thunk iterations)
  (sb-ext:gc :full t)
  (let ((start (sb-ext:get-bytes-consed)))
    (dotimes (i iterations) (funcall thunk))
    (round (/ (- (sb-ext:get-bytes-consed) start) iterations))))

(prove:deftest test-render-transform-no-allocation
  ;; REGRESSION: building an object's render transform every frame used to
  ;; allocate a fresh 4x4 matrix (~80 bytes) per drawable because
  ;; OBB-RENDER-TRANSFORM called the allocating MATRIX*. At 60fps with many
  ;; visible objects this was the dominant source of per-frame GC pressure
  ;; (render dominated the frame's allocation). With an output buffer it must
  ;; allocate nothing.
  (let ((obb (make-instance 'obb :width 32.0 :height 32.0))
        (scratch (identity-matrix)))
    ;; warm up / JIT
    (obb-render-transform obb scratch)
    ;; sb-ext:get-bytes-consed is process-wide, so a handful of bytes/call can
    ;; leak in from other threads (slynk, finalizers). The regression we guard
    ;; against allocated a full 80-byte matrix *every* call, so anything under a
    ;; few bytes/call proves the per-call matrix allocation is gone.
    (let ((per-call (%bytes-consed (lambda () (obb-render-transform obb scratch))
                                   100000)))
      (prove:ok (< per-call 16)
                (format nil "obb-render-transform with a buffer does not allocate per call (got ~A bytes/call, regression was 80)"
                        per-call)))
    ;; sanity: the buffered result equals the allocating result
    (prove:is (obb-render-transform obb scratch)
              (obb-render-transform obb)
              "buffered render transform matches the allocating version"
              :test #'equalp)))

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
        #+nil
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
    #+nil
    (prove:is (collidep phantom2 triangle)
        nil
        "phantoms collide with polygons")
    #+nil
    (prove:is (collidep triangle phantom2)
        nil
        "polygons do not collide with phantoms")))
