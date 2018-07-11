(in-package :recurse.vert/unit-test)

(defun %point-vectors-equal (result-vector expected-vector &optional message)
  "Test to ensure the two vectors contain the same points in the same order.
Runs prove:pass or prove:fail based on result."
  (declare (type (array point) result-vector expected-vector))
  (unless (= (length expected-vector) (length result-vector))
    (fail (format nil "Expected vector of size ~A got ~A"
                  (length expected-vector) result-vector)))
  (loop for expected-point across expected-vector
     for result-point across result-vector do
       (unless (point= expected-point result-point)
         (fail (if message
                   (format nil
                           "~A~%~A is expected to be ~A"
                           message result-point expected-point)
                   (format nil
                           "~A is expected to be ~A"
                           result-point expected-point)))
         (return nil))
     finally
       (return (pass (or message (format nil
                                         "~A is expected to be ~A"
                                         result-vector expected-vector))))))

(deftest aabb-local-world-points
  (let ((aabb (make-instance 'aabb
                             :width 10 :height 37
                             :x 12 :y 95 :z 3)))
    (%point-vectors-equal (local-points aabb)
                          (vector (make-point :x 0  :y 0)
                                  (make-point :x 10  :y 0)
                                  (make-point :x 10  :y 37)
                                  (make-point :x 0  :y 37))
                          "local space points")
    (%point-vectors-equal (world-points aabb)
                          (vector (make-point :x 12  :y 95 :z 3)
                                  (make-point :x 22 :y 95 :z 3)
                                  (make-point :x 22 :y 132 :z 3)
                                  (make-point :x 12 :y 132 :z 3))
                          "world space points")))

(deftest collision-aabb-aabb
  (let ((rect1 (make-instance 'aabb :width 30 :height 50 :x 0 :y 0))
        (rect2 (make-instance 'aabb :width 10 :height 30 :x 30.00001 :y 50.00001)))
    (prove:is (collidep rect1 rect2) nil "no collision")
    (move rect1 1 0) ;; move along x axis
    (prove:is (collidep rect1 rect2) nil "no collision")
    (move rect1 -1 0) ;; move back
    (move rect1 0 1)  ;; move along y axis
    (prove:is (collidep rect1 rect2) nil "no collision")
    (move rect1 0 -1) ;; move back

    (move rect1 1 1) ;; move along x and y axis
    (prove:is (collidep rect1 rect2) T "collision")
    (move rect1 0 0 1) ;; move to another 2d plane
    (prove:is (collidep rect1 rect2) nil "no collision")))
