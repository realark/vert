(in-package :recurse.vert/unit-test)

(deftest render-interpolation
  (let ((obj (make-instance 'test-object
                            :width 10 :height 10
                            :x 20 :y 30)))
    (prove:is-values (recurse.vert::%interpolate obj 10 20 0.3)
                     '(10 20)
                     "No initial interpolation"
                     :test #'equalp)
    (prove:is-values (recurse.vert::%interpolate obj 20 30 0.5)
                     '(15 25)
                     "50 percent between updates"
                     :test #'equalp)
    (prove:is-values (recurse.vert::%interpolate obj 30 40 0.0)
                     '(20 30)
                     "0 percent between updates"
                     :test #'equalp)

    ;; not actually possible (game loop will wrap before reaching 1), but good to test
    (prove:is-values (recurse.vert::%interpolate obj 40 50 1.0)
                     '(40 50)
                     "100 percent between updates"
                     :test #'equalp)))
