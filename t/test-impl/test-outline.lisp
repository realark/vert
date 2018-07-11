(in-package :recurse.vert/test)

(defclass test-outline (composed-object aabb)
  ()
  (:documentation "Hollow rectangle"))

(defmethod initialize-instance :after ((test-outline test-outline) &rest args)
  (declare (ignore args))
  (with-accessors ((x x) (y y) (z z)
                   (width width)
                   (height height))
      test-outline
    (setf (slot-value test-outline 'sub-objects)
          (make-array 4 :fill-pointer 0 :adjustable nil))
    (recurse.vert::add-object test-outline (make-instance 'aabb
                                                          :x x
                                                          :y y
                                                          :z z
                                                          :width 1
                                                          :height height))
    (recurse.vert::add-object test-outline (make-instance 'aabb
                                                          :x x
                                                          :y y
                                                          :z z
                                                          :width width
                                                          :height 1))
    (recurse.vert::add-object test-outline (make-instance 'aabb
                                                          :x (+ width x)
                                                          :y y
                                                          :z z
                                                          :width 1
                                                          :height height))
    (recurse.vert::add-object test-outline (make-instance 'aabb
                                                          :x x
                                                          :y (+ height y)
                                                          :z z
                                                          :width width
                                                          :height 1))))
