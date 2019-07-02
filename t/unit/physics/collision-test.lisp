(in-package :recurse.vert/unit-test)

(recurse.vert::defcollision-resolution reset-resolution ((moving-object game-object)
                                                         (stationary-object game-object)
                                                         &key original-position)
  (setf (x moving-object) (point-x original-position)
        (y moving-object) (point-y original-position)
        (z moving-object) (point-z original-position)))

(deftest collision-check
  "Test with-collision-check macro"
  (let ((context (make-instance 'physics-context-2d
                                :spatial-partition (make-instance 'quadtree)))
        (moving-object (make-instance 'test-object :width 1 :height 1))
        (original-position (make-point)))
    (recurse.vert::start-tracking (spatial-partition context) (make-instance 'test-object
                                                                             :width 10 :height 10
                                                                             :x 20
                                                                             :y 5))
    (recurse.vert::start-tracking (spatial-partition context) moving-object)

    (setf (point-x original-position) (x moving-object)
          (point-y original-position) (y moving-object)
          (point-z original-position) (z moving-object))
    (with-collision-check (moving-object context)
      (:position-update
       (setf (x moving-object) 18)
       (setf (y moving-object) 10))
      (:on-collision stationary-object
                     (reset-resolution moving-object
                                       stationary-object
                                       :original-position original-position)))
    (prove:is (x moving-object) 18 "x move legal" :test #'float=)
    (prove:is (y moving-object) 10 "y move legal" :test #'float=)
    (prove:is (num-collisions moving-object) 0 "no collisions")

    (setf (point-x original-position) (x moving-object)
          (point-y original-position) (y moving-object)
          (point-z original-position) (z moving-object))
    (with-collision-check (moving-object context)
      (:position-update
       (setf (x moving-object) 23)
       (setf (y moving-object) 13))
      (:on-collision stationary-object
                     (reset-resolution moving-object
                                       stationary-object
                                       :original-position original-position)))
    (prove:is (x moving-object) 18 "x move illegal" :test #'float=)
    (prove:is (y moving-object) 10 "y move illegal" :test #'float=)
    (prove:is (num-collisions moving-object) 1 "one collision")))

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
