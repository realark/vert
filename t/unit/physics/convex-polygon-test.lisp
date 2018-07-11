(in-package :recurse.vert/unit-test)

(deftest convex-polygon-local-world-points
  (let ((polygon (make-instance 'convex-polygon
                                :x 10
                                :y 13
                                :z 2
                                :local-points (vector (make-point :x 1 :y 1)
                                                      (make-point :x 3 :y 1)
                                                      (make-point :x 2 :y 2)))))
    (%point-vectors-equal (local-points polygon)
                          (vector (make-point :x 1  :y 1)
                                  (make-point :x 3 :y 1)
                                  (make-point :x 2  :y 2))
                          "local space points")
    (%point-vectors-equal (world-points polygon)
                          (vector (make-point :x 11 :y 14 :z 2)
                                  (make-point :x 13 :y 14 :z 2)
                                  (make-point :x 12 :y 15 :z 2))
                          "world space points")
    (setf (rotation polygon) (recurse.vert::deg->rad 90d0))
    (%point-vectors-equal (local-points polygon)
                          (vector (make-point :x 1  :y 1)
                                  (make-point :x 3 :y 1)
                                  (make-point :x 2  :y 2))
                          "local space unrotated")
    (%point-vectors-equal (world-points polygon)
                          (vector (make-point :x 11.5 :y 14.5 :z 2)
                                  (make-point :x 11.5 :y 12.5 :z 2)
                                  (make-point :x 12.5 :y 13.5 :z 2))
                          "world space rotated")))

(deftest collision-polygon-aabb
  (let ((polygon (make-instance 'convex-polygon
                                :rotation (recurse.vert::deg->rad 90d0)
                                :local-points (vector (make-point :x 0 :y 0)
                                                      (make-point :x 2 :y 0)
                                                      (make-point :x 1 :y 4))))
        (aabb (make-instance 'aabb :x 3 :y 2 :width 1 :height 1)))
    ;; poly rotated about: 1,,2
    ;; polygon is a triangle with world coords:
    ;; -1,1
    ;;      3,2
    ;; -1,3
    (is (collidep polygon aabb) T "collide right point")
    (setf (x aabb) 3.1
          (y aabb) 2)
    (is (collidep polygon aabb) nil "no collision")
    (setf (x aabb) 3
          (y aabb) 2.1)
    (is (collidep polygon aabb) nil "no collision")
    (setf (x aabb) -2
          (y aabb) 3)
    (is (collidep polygon aabb) T "collide bottom point")
    (setf (x aabb) -2
          (y aabb) 0)
    (is (collidep polygon aabb) T "collide top point")))
