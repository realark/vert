(in-package :recurse.vert/unit-test)

(deftest composable-object
  (let* ((sub1 (make-instance 'test-object
                              :x 0 :y 0
                              :width 5 :height 10))
         (sub2 (make-instance 'test-object
                              :x 5 :y 0
                              :width 5 :height 10))
         (composed (make-instance 'composed-object
                                  :objects (vector sub1 sub2)))
         (obj (make-instance 'test-object :x 9 :y 9)))
    (update composed 10 nil)
    (is (= 1
           (method-invoke-count sub1 "update")
           (method-invoke-count sub1 "update"))
        T "update delegates")
    (is (collidep composed obj) T "collision delegates")

    (render composed .5 nil nil)
    (is (= 1
           (method-invoke-count sub1 "render")
           (method-invoke-count sub1 "render"))
        T "render delegates")

    (load-resources composed nil)
    (is (= 1
           (method-invoke-count sub1 "load-resources")
           (method-invoke-count sub1 "load-resources"))
        T "load-resources delegates")

    (let ((release-count (method-invoke-count sub1 "release-resources")))
      (release-resources composed)
      (is (= (+ 1 release-count)
             (method-invoke-count sub1 "release-resources")
             (method-invoke-count sub1 "release-resources"))
          T "release-resources delegates"))

    (recycle composed)
    (is (= 1
           (method-invoke-count sub1 "recycle")
           (method-invoke-count sub1 "recycle"))
        T "recycle delegates")))
