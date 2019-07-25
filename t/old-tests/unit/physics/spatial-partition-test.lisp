(in-package :recurse.vert/unit-test)

(defun %get-possible-overlaps (partition object)
  (let ((neighbors (make-array 20 :adjustable T :fill-pointer 0)))
    (do-neighbors (object partition neighbor)
      (vector-push-extend neighbor neighbors))
    neighbors))


(defun %partitions-to-test ()
  (list (make-instance 'recurse.vert::layered-quadtree
                       :width 100
                       :height 100
                       :max-depth 2
                       :max-objects 4)))

(defun %partition-lookups (partition)
  (is (not (null partition)) T (format nil "Testing ~A" partition))
  (let ((n1 (make-instance 'AABB :width 1 :height 1 :x 10 :y 10))
        (n2 (make-instance 'AABB :width 1 :height 1 :x 11 :y 10))
        (n3 (make-instance 'AABB :width 1 :height 1 :x 90 :y 90))
        (n4 (make-instance 'AABB :width 1 :height 1 :x 91 :y 90))
        (n5 (make-instance 'AABB :width 1 :height 1 :x 105 :y 105)))
    (start-tracking partition n1)
    (start-tracking partition n2)
    (start-tracking partition n3)
    (start-tracking partition n4)
    ;; Insert outside of right boundary
    (start-tracking partition n5)

    ;; Move N1 outside of left boundary
    (setf (x n1) -10)
    (setf (y n1) -10)

    (let ((overlaps-right (%get-possible-overlaps partition
                                                  (make-instance 'AABB
                                                                 :width 30
                                                                 :height 30
                                                                 :x 85
                                                                 :y 85)))
          (overlaps-left (%get-possible-overlaps partition
                                                 (make-instance 'AABB
                                                                :width 30
                                                                :height 30
                                                                :x -10
                                                                :y -10))))
      ;; check right side
      (is (length overlaps-right) 3
          "At least three possible overlaps on right boundary."
          :test #'>=)
      (is (find n3 overlaps-right) n3 "N3 is a possible right overlap.")
      (is (find n4 overlaps-right) n4 "N4 is a possible right overlap.")
      (is (find n5 overlaps-right) n5 "N5 is a possible right overlap.")
      ;; check left side
      (is (length overlaps-left) 2
          "At least two possible overlaps on left boundary."
          :test #'>=)
      (is (find n1 overlaps-left) n1 "N1 is a possible overlap.")
      (is (find n2 overlaps-left) n2 "N2 is a possible overlap."))
    (let* ((n6 (make-instance 'test-object
                              :x -20
                              :y -20
                              :width 1000
                              :height 1000))
           (all-objects (progn
                          (start-tracking partition n6)
                          (delete-duplicates (%get-possible-overlaps partition n6)))))
      (is (length all-objects) 5 "All objects overlapped by n6"))))

(deftest partition-lookups
  (mapcar #'%partition-lookups (%partitions-to-test)))

(defun %partition-iteration (partition)
  (is (not (null partition)) T (format nil "Testing ~A" partition))
  (let ((test-objects (list
                       (make-instance 'test-object :width 1 :height 1 :x 10 :y 10)
                       (make-instance 'test-object :width 1 :height 1 :x 11 :y 10)
                       (make-instance 'test-object :width 1 :height 1 :x 90 :y 90)
                       (make-instance 'test-object :width 1 :height 1 :x 91 :y 90)
                       (make-instance 'test-object :width 1 :height 1 :x 105 :y 105)
                       (make-instance 'test-object :width 10 :height 10 :x 25 :y 25)
                       (make-instance 'test-object :width 10 :height 10 :x 45 :y 45)
                       (make-instance 'test-object :width 10 :height 10 :x 45 :y 65))))
    (loop for object in test-objects do
         (start-tracking partition object))
    (flet ((test-expected-objects (&optional message)
             (let ((iterated-objects (list)))
               (do-spatial-partition (object partition)
                 (push object iterated-objects))
               (is (length iterated-objects) (length (remove-duplicates iterated-objects))
                   (format nil "~A: No duplicates when iterating partition." message)
                   :test #'=)
               (is (length (intersection iterated-objects test-objects)) (length iterated-objects)
                   (format nil "~A: All objects iterated." message)
                   :test #'=))))
      (test-expected-objects "All objects inserted")

      (let ((obj-to-delete (elt test-objects 3)))
        (setf test-objects (delete obj-to-delete test-objects))
        (stop-tracking partition obj-to-delete)
        (test-expected-objects "Non-iterating deletion"))

      (let ((obj-to-delete (elt test-objects 3)))
        (setf test-objects (delete obj-to-delete test-objects))
        (do-spatial-partition (obj partition)
          (when (eq obj obj-to-delete)
            (stop-tracking partition obj)))
        (test-expected-objects "Iterating deletion"))
      (let ((obj-to-move (elt test-objects 0)))
        (do-spatial-partition (obj partition)
          (update obj 10 nil)
          (when (eq obj-to-move obj)
            (setf (x obj) 90
                  (y obj) 90)))
        (is (method-invoke-count obj-to-move "update") 1 "Moved object only invoked once.")))))

(deftest partition-iteration
  (mapcar #'%partition-iteration (%partitions-to-test)))

(defun %partition-z-layer (partition)
  (let ((obj1 (make-instance 'test-object :width 10 :height 10 :x 10 :y 10))
        (obj2 (make-instance 'test-object :width 5 :height 5 :x 18 :y 18))
        (obj3 (make-instance 'test-object :width 5 :height 5 :x 15 :y 15)))
    (loop for object in (list obj1 obj2 obj3) do
         (start-tracking partition object))
    (flet ((is-vector (got expected doc)
             (unless (= (length (remove-duplicates expected)) (length expected))
               (fail (format nil "Duplicates in assertion vector: ~A" doc)))
             (unless (= (length got) (length expected))
               (fail (format nil "lists are not the same length: ~A" doc)))
             (loop for expected-obj across expected do
                  (unless (find expected-obj got)
                    (fail (format nil "~A:: Missing element: ~A" doc expected-obj))
                    (return))
                finally (pass doc))))
      (is-vector (%get-possible-overlaps partition obj3)
                 (vector obj1 obj2)
                 "Obj3 overlaps 1 and 2")
      (setf (z obj3) 1)
      (is-vector (%get-possible-overlaps partition obj3)
                 (vector)
                 "Obj3 has no overlaps at z level")

      (setf (z obj3) 0)
      (is-vector (%get-possible-overlaps partition obj3)
                 (vector obj1 obj2)
                 "Obj3 overlaps 1 and 2 when moved back to 0 z-layer"))))

(deftest partition-z-layer
  (mapcar #'%partition-z-layer (%partitions-to-test)))

(deftest quadtree-split-preserves-z
  (let ((tree (make-instance 'quadtree
                             :z -1
                             :max-objects 1
                             :max-depth 3))
        (obj1 (make-instance 'test-object :width 10 :height 10 :x 10 :y 10 :z -1))
        (obj2 (make-instance 'test-object :width 5 :height 5 :x 18 :y 18 :z -1)))
    (start-tracking tree obj1)
    (start-tracking tree obj2)
    (loop for child across (recurse.vert::children tree) do
         (is (z child) -1 "z layer must be preserved for child nodes"))))
