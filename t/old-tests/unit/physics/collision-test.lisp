(in-package :recurse.vert/test)

(defclass phantom-obb (phantom obb) ())

(deftest collision-phantoms
  (let ((phantom1 (make-instance 'phantom-obb
                                 :x 10
                                 :y 10
                                 :width 10
                                 :height 10))
        (phantom2 (make-instance 'phantom-obb
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
    (is (collidep phantom1 box1)
        t
        "phantoms collide with normal objects")
    (is (collidep box1 phantom1)
        nil
        "normal objects do not collide with phantoms")
    (is (collidep phantom1 phantom2)
        nil
        "phantoms do not collide with each other.")
    (is (collidep phantom2 triangle)
        nil
        "phantoms collide with polygons")
    (is (collidep triangle phantom2)
        nil
        "polygons do not collide with phantoms")))
