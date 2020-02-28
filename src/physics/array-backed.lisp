(in-package :recurse.vert)

(defclass array-backed (spatial-partition)
  ((objects :initform (make-array 1000
                                  :adjustable t
                                  :fill-pointer 0
                                  :initial-element nil)))
  (:documentation "A spatial partition which is simply a big, flat array."))


(defmethod start-tracking ((array-backed array-backed) (object game-object))
  (with-slots (objects) array-backed
    (unless (find object objects)
      (vector-push-extend object objects))))

(defmethod stop-tracking ((array-backed array-backed) (object game-object))
  (with-slots (objects) array-backed
    (setf objects (delete object objects))))

(defmethod partition-clear ((array-backed array-backed))
  (with-slots (objects) array-backed
    (loop :for i :from 0 :below (length objects) :do
       ;; explicitly null out objects so we don't create a hard reference
         (setf (elt objects i) nil))
    (setf (fill-pointer objects) 0)))
