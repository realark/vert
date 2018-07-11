(in-package :recurse.vert/unit-test)

(deftest touch-tracker
  (let ((tt (make-instance 'obb-touch-tracker
                           :x 100
                           :y 100
                           :width 10 :height 10))
        (obj1 (make-instance 'test-object
                             :x 100
                             :y (- 99 recurse.vert::*collision-precision*)))
        (obj2 (make-instance 'test-object
                             :x 101
                             :y (+ 110 recurse.vert::*collision-precision*)))
        (obj3 (make-instance 'test-object
                             :x (+ 110 recurse.vert::*collision-precision*)
                             :y 105)))
    (collision tt obj1)
    (collision tt obj2)
    (collision tt obj3)
    (is (objects-touching tt :north) (list obj1)
        "obj1 touching north region" :test #'equalp)
    (is (objects-touching tt :south) (list obj2)
        "obj2 touching south region" :test #'equalp)
    (is (objects-touching tt :east) (list obj3)
        "obj3 touching east region" :test #'equalp)
    (is (sort (objects-touching tt :all)
              (lambda (object-a object-b)
                (< (x object-a) (x object-b))))
        (list obj1 obj2 obj3)
        "obj1 2 and 3 in union region" :test #'equalp)

    (setf (x tt) 99)
    (is (objects-touching tt :north) (list obj1)
        "obj1 touching north region" :test #'equalp)
    (is (objects-touching tt :south) (list obj2)
        "obj2 touching south region" :test #'equalp)
    (is (objects-touching tt :east) '()
        "obj3 not touching east region" :test #'equalp)
    (is (sort (objects-touching tt :all)
              (lambda (object-a object-b)
                (< (x object-a) (x object-b))))
        (list obj1 obj2)
        "obj1 and 2 in union region" :test #'equalp)))

(deftest touch-track-after-collide
  (let ((world (make-instance 'physics-context-2d
                              :spatial-partition (make-instance 'quadtree)))
        (tt (make-instance 'obb-touch-tracker
                           :x 100
                           :y 100
                           :width 10 :height 10))
        (obj (make-instance 'test-object
                            :x 100
                            :y 98.9)))
    (start-tracking (spatial-partition world) obj)
    (start-tracking (spatial-partition world) tt)
    (loop with down-vec = (make-vector2 :y 0.01)
       for i from 0 below 10 do
         (apply-vector obj down-vec)
         (update obj 10 world))
    (is (objects-touching tt) (list obj) "OBJ moved into TT")))

(deftest touch-tracker-after-collision-resolution
  "Touch-Tracker can still track obj after collision resolution"
  (let ((world (make-instance 'physics-context-2d
                              :spatial-partition (make-instance 'quadtree)))
        (tt (make-instance 'obb-touch-tracker
                           :x 10
                           :y (- 100 10 1 recurse.vert::*collision-precision*)
                           :width 10 :height 10))
        (obj (make-instance 'test-object
                            :x 10
                            :y (- 100 1 (/ recurse.vert::*collision-precision* 2))))
        (ground (make-instance 'test-object
                               :x 0
                               :y 100
                               :width 100
                               :height 100)))
    (start-tracking (spatial-partition world) tt)
    (start-tracking (spatial-partition world) obj)
    (start-tracking (spatial-partition world) ground)

    ;; obj should be tightly packed between tt and ground. Unable to actually move up or down
    (loop with up-vec = (make-vector2 :y -0.01)
       for i from 0 below 10 do
         (apply-vector obj up-vec)
         (update obj 10 world))
    (is (objects-touching tt) (list obj) "TT initially touching obj")

    (loop with down-vec = (make-vector2 :y 0.01)
       and obj-orig-y = (y obj)
       for i from 0 below 10 do
         (apply-vector obj down-vec)
         (update obj 10 world)
       finally (is (y obj) obj-orig-y "Obj position unchanged"))

    (is (objects-touching tt) (list obj) "TT still touching obj after obj-ground collision resolution")))
