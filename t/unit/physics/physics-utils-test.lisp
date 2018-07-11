(in-package :recurse.vert/unit-test)

(deftest point-distance
  (let ((p1 (make-point :x -10 :y -17 :z -2))
        (p2 (make-point :x 13 :y 0 :z 3)))
    (is (distance-between p1 p2) 29.03446
        "Precise distance between points."
        :test #'float=)))
