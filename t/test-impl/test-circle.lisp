(in-package :recurse.vert/test)

(defclass test-circle (test-object)
  ((path-to-sprite
    :initform (test-resource-path "circle.png")
    :reader path-to-sprite
    :allocation :class))
  (:documentation "OBB Circle used for testing"))
