;;;; OBB which can be rotated about it's local origin.

(in-package :recurse.vert)

;; this is a hack to allow obb to extend convex-polygon
;; without forcing user to specify :local-points
;; dummy local points which will be replaced in obb initializer
(defparameter %%dummy-local-points
  (vector (make-point)
          (make-point :x 1)
          (make-point :y 1)))

(defclass obb (convex-polygon)
  ((local-points :initform %%dummy-local-points))
  (:documentation "O(riented)B(ounding)B(ox). A rectangle which can be rotated."))

(defmethod initialize-instance :after ((obb obb) &key width height)
  (with-slots (local-points world-points (slot-w width) (slot-h height)) obb
    (when width
      (setf slot-w (coerce width 'world-dimension)))
    (when height
      (setf slot-h (coerce height 'world-dimension)))
    (setf local-points
          (vector (make-point :x 0 :y 0)
                  (make-point :x slot-w :y 0)
                  (make-point :x slot-w :y slot-h)
                  (make-point :x 0 :y slot-h)))
    (setf world-points
          (make-array
           (length local-points)
           :initial-element *origin*
           :element-type 'point))
    (loop for i from 0 below (length local-points) do
         (setf (elt world-points i) (make-point)))
    (%update-polygon-world-points obb)
    (%update-polygon-bounding-box obb)))
