(in-package :recurse.vert/unit-test)

(deftest obb-local-world-points
  (let ((obb (make-instance 'obb
                            :rotation 90
                            :width 3
                            :height 6
                            :x 10
                            :y 15
                            :z 1))
        (polygon (make-instance 'convex-polygon
                                :rotation 90
                                :x 10
                                :y 15
                                :z 1
                                :local-points
                                (vector (make-point :x 0  :y 0)
                                        (make-point :x 3 :y 0)
                                        (make-point :x 3 :y 6)
                                        (make-point :x 0  :y 6)))))
    ;; polygon and obb should be exactly the same
    (%point-vectors-equal (local-points obb)
                          (local-points polygon)
                          "local space points")
    (%point-vectors-equal (world-points obb)
                          (world-points polygon)
                          "initial world space points")
    (setf (rotation obb) 183
          (rotation polygon) 183)
    (%point-vectors-equal (world-points obb)
                          (world-points polygon)
                          "rotated world space points")))
