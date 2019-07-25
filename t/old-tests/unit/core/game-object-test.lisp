(in-package :recurse.vert/unit-test)

(deftest game-object-pinning
  ;; pin xyz
  (let* ((x-diff 2) (y-diff 7) (z-diff 4)
         (rect1 (make-instance 'test-object :x 10 :y 10 :z 0 :width 20 :height 10))
         (rect2 (make-instance 'test-object
                               :x (+ (x rect1) x-diff)
                               :y (+ (y rect1) y-diff)
                               :z (+ (z rect1) z-diff)
                               :width 5
                               :height 5)))
    (pin-to rect2 rect1)
    (move rect1 5 12 3)
    (setf (rotation rect1) (recurse.vert::deg->rad 20d0))
    (is (rotation rect2) (recurse.vert::deg->rad 20d0) "pin follows rotation" :test #'=)
    (is (x rect1) (- (x rect2) x-diff) "pin follows x" :test #'=)
    (is (y rect1) (- (y rect2) y-diff) "pin follows y" :test #'=)
    (is (z rect1) (- (z rect2) z-diff) "pin follows z" :test #'=)
    (unpin-from rect2 rect1)
    (move rect1 1 1 1)
    (setf (rotation rect1) (recurse.vert::deg->rad 10d0))
    (is (rotation rect2) (recurse.vert::deg->rad 20d0) "unpinned rotation" :test #'=)
    (is (x rect1) (- (x rect2) x-diff -1) "unpinned x" :test #'=)
    (is (y rect1) (- (y rect2) y-diff -1) "unpinned y" :test #'=)
    (is (z rect1) (- (z rect2) z-diff -1) "unpinned z" :test #'=)))
