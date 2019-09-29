(in-package :recurse.vert/test)

(deftest partition-iteration
  (let ((partition (make-instance 'quadtree
                                  :width 100
                                  :height 100))
        (obj-hash (make-hash-table :test #'eq))
        (num-objects 100))
    (loop :for i :from 0 :below num-objects :do
         (let ((obj (make-instance 'obb
                                   :x i
                                   :y i
                                   :width 2
                                   :height 2)))
           (start-tracking partition obj)
           (setf (gethash obj obj-hash) 0)))
    ;; test updating all objects
    (do-spatial-partition (obj partition)
      (setf (gethash obj obj-hash)
            (+ 1 (gethash obj obj-hash))))
    (loop :for obj :being :the hash-keys :of obj-hash
       :using (hash-value update-count) :do
         (is update-count
             1
             (format nil "Object ~A should be updated exactly once" obj)))
    ;; test updating specific region
    (let ((min-x 0.0)
          (max-x 50.0)
          (min-y 25.0)
          (max-y 75.0))
      (do-spatial-partition (obj partition
                                 :min-x min-x :max-x max-x
                                 :min-y min-y :max-y max-y)
        (setf (gethash obj obj-hash)
              (+ 1 (gethash obj obj-hash))))

      (loop :for obj :being :the hash-keys :of obj-hash
         :using (hash-value update-count) :do
           (let ((expected-update-count (if (and (or (<= min-x (x obj) max-x)
                                                     (<= min-x (+ (x obj) (width obj)) max-x))
                                                 (or (<= min-y (y obj) max-y)
                                                     (<= min-y (+ (y obj) (height obj)) max-y)))
                                            2
                                            1)))
             (is update-count
                 expected-update-count
                 (format nil "Object in boundary ~A should be updated exactly once" obj)))))))
