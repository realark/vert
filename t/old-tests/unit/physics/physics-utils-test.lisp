(in-package :recurse.vert/test)

(deftest point-distance
  (let ((p1 (vector3 -10f0 -17f0 -2f0))
        (p2 (vector3 13f0 0f0 3f0)))
    (is (distance-between p1 p2) 29.03446
        "Precise distance between points."
        :test #'float=)))
