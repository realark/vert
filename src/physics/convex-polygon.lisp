(in-package :recurse.vert)

(defclass convex-polygon (obb)
  ((local-points
    :initarg :local-points
    :initform (error ":local-points required")
    :documentation "The polygon's local points."))
  (:documentation "A convex polygon with [3,) points."))

(defmethod initialize-instance :after ((polygon convex-polygon) &rest args)
  (declare (ignore args))
  (with-slots (local-points) polygon
    (assert (and (>= (length local-points) 3)
                 (typep local-points '(simple-array vector3))))
    (loop :for i :from 0 :below (length local-points) :do
         (let ((current (elt local-points i))
               (previous (if (= i 0)
                             (elt local-points (1- (length local-points)))
                             (elt local-points (1- i)))))
           (assert (typep (distance-between current previous) 'world-dimension)))))
  (%initialize-polygon-width-height polygon))

(defun %initialize-polygon-width-height (polygon)
  (declare (type convex-polygon polygon))
  (loop :with local-points = (local-points polygon)
     :with right = (elt local-points 0)
     :and down = (elt local-points 0)
     :for point :across local-points :do
       (when (or (< (x point) 0)
                 (< (y point) 0))
         (error "polygon cannot have a negative local-point"))
       (when (> (x point) (x right))
         (setf right point))
       (when (> (y point) (y down))
         (setf down point))
     :finally
       (setf (width polygon) (coerce (x right) 'world-dimension)
             (height polygon) (coerce (y down) 'world-dimension)))
  polygon)

(defmethod local-points ((polygon convex-polygon))
  (slot-value polygon 'local-points))

(defcollision ((poly convex-polygon) (obj obb))
  (declare (optimize (speed 3)))
  (convex-poly-collidep poly obj))
