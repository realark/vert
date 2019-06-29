;;;; OBB which can be rotated about it's local origin.

(in-package :recurse.vert)

;; this is a hack to allow obb to extend convex-polygon
;; without forcing user to specify :local-points
;; dummy local points which will be replaced in obb initializer
(defvar %%dummy-local-points
  (vector (vector3 0.0 0.0 0.0)
          (vector3 1.0 0.0 0.0)
          (vector3 0.0 1.0 0.0)))

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
          (vector (vector3 0.0 0.0 0.0)
                  (vector3 slot-w 0.0 0.0)
                  (vector3 slot-w slot-h 0.0)
                  (vector3 0.0 slot-h 0.0)))
    (setf world-points
          (make-array
           (length local-points)
           :initial-element *origin*
           :element-type 'vector3))
    (loop :for i :from 0 :below (length local-points) do
         (setf (elt world-points i) (vector3 0.0 0.0 0.0)))
    (%update-polygon-world-points obb)
    (%update-polygon-bounding-box obb)))
