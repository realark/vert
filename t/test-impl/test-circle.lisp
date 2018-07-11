(in-package :recurse.vert/test)

(defclass test-circle (test-object)
  ((path-to-image
    :initform (test-resource-path "circle.png")
    :reader path-to-image
    :allocation :class))
  (:documentation "OBB Circle used for testing"))
