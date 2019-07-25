(in-package :recurse.vert/unit-test)

(deftest linear-motion
  (let ((context (make-instance 'physics-context-2d
                                ;; just like physics class
                                :friction-x 1.0
                                :drag-y 1.0
                                :max-velocity-x nil
                                :max-velocity-y nil
                                :spatial-partition (make-instance 'quadtree)))
        (moving-object (make-instance 'test-object :width 1 :height 1))
        (a-x 4)
        (a-y 7)
        (total-time-seconds 30))
    (start-tracking (spatial-partition context) moving-object)
    (loop with total-time-ms = (* total-time-seconds 1000)
       with timestep-ms = 17
       for current-time from 0 to total-time-ms by timestep-ms do
         (apply-vector moving-object (make-acceleration-vector-seconds :x a-x :y a-y))
         (update moving-object timestep-ms context)
       finally
         (update moving-object timestep-ms context))

    ;; d = v0 * t + .5 * a * t^2
    ;; v0 = 0
    ;; using euler integration so this won't be completely accurate
    (is (abs (- (x moving-object) (* .5 a-x (expt total-time-seconds 2))))
        3
        "x displacement correct"
        :test #'<=)
    (is (abs (- (y moving-object) (* .5 a-y (expt total-time-seconds 2))))
        3
        "y displacement correct"
        :test #'<=)))

(deftest linear-resolution
  (let ((stationary-object (make-instance 'test-object
                                          :x 100 :y 100
                                          :width 1000
                                          :height 1000))
        (moving-top (make-instance 'test-object
                                   :width 10
                                   :height 10
                                   :x 150
                                   :y 89.999))
        (moving-right (make-instance 'test-object
                                     :width 10
                                     :height 10
                                     :x 89.99
                                     :y 150))
        ;; (moving-corner (make-instance 'test-object))
        (timestep-ms 10)
        (context (make-instance 'physics-context-2d
                                :friction-x 1.0
                                :drag-y 1.0
                                :max-velocity-x nil
                                :max-velocity-y nil
                                :spatial-partition (make-instance 'quadtree))))
    (start-tracking (spatial-partition context) moving-top)
    (start-tracking (spatial-partition context) stationary-object)

    (apply-vector moving-top (make-acceleration-vector-seconds :x 100 :y 100))
    (update moving-top timestep-ms context)
    (update moving-top timestep-ms context)
    (is (velocity-y moving-top) 0 "Y movement halted.")
    (is (velocity-x moving-top) (* timestep-ms (/ 100 (expt 10.0 6))) "X movement unchanged.")

    (apply-vector moving-right (make-acceleration-vector-seconds :x 100.1 :y 100))
    (update moving-right timestep-ms context)
    (update moving-right timestep-ms context)
    (is (velocity-y moving-right) (* timestep-ms (/ 100 (expt 10.0 6))) "Y movement unchanged.")
    (is (velocity-x moving-right) 0 "X movement halted")

    (let ((outline (make-instance 'test-outline
                                  :x 0
                                  :y 0
                                  :width 30
                                  :height 30))
          (corner-object (make-instance 'test-object
                                        :x 19.999
                                        :y 19.999
                                        :width 10
                                        :height 10)))
      (start-tracking (spatial-partition context) corner-object)
      (start-tracking (spatial-partition context) outline)
      (apply-vector corner-object (make-acceleration-vector-seconds :x 100 :y 100))
      (update corner-object timestep-ms context)
      (update corner-object timestep-ms context)
      (is (velocity-y corner-object) 0 "Y movement halted.")
      (is (velocity-x corner-object) 0 "X movement halted"))))
