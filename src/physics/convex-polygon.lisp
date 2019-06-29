(in-package :recurse.vert)

(defclass convex-polygon (aabb)
  ((local-points
    :initarg :local-points
    :initform (error ":local-points required")
    :documentation "The polygon's local points.")
   (local-center
    :initform nil
    :documentation "Polygon's center in local space")
   (rotation :initarg :rotation
             :reader rotation
             :initform 0f0
             :type rotation-radians
             :documentation "Polygon's rotation in degrees. Rotated about its center.")
   (bounding-box :reader bounding-box
                 :type aabb
                 :initform (make-instance 'aabb)))
  (:documentation "A convex polygon with [3,) points."))

(defmethod initialize-instance :after ((polygon convex-polygon) &rest args)
  (declare (ignore args))
  (with-slots (rotation local-points world-points) polygon
    (setf rotation (coerce rotation 'rotation-radians))
    (assert (and (>= (length local-points) 3)
                 (typep local-points '(simple-array vector3))))
    (loop for i from 0 below (length local-points) do
         (let ((current (elt local-points i))
               (previous (if (= i 0)
                             (elt local-points (1- (length local-points)))
                             (elt local-points (1- i)))))
           (assert (typep (distance-between current previous) 'world-dimension))))
    (setf world-points
          (make-array
           (length local-points)
           :initial-element *origin*
           :element-type 'vector3))
    (loop for i from 0 below (length local-points) do
         (setf (elt world-points i) (vector3 0.0 0.0 0.0))))
  (%initialize-polygon-width-height polygon)
  (%update-polygon-world-points polygon)
  (%update-polygon-bounding-box polygon))

(defmethod print-object ((polygon convex-polygon) out)
  (print-unreadable-object (polygon out :type t)
    (format out "(~A,~A,~A)(~Ax~A)(r~A-degrees)" (float (x polygon)) (float (y polygon)) (float (z polygon))
            (width polygon) (height polygon) (rad->deg (rotation polygon)))))

(defmethod (setf rotation) (value (polygon convex-polygon))
  (setf (slot-value polygon 'rotation) (coerce (mod value tau) 'rotation-radians)))

(defmethod object-moved-all :after ((polygon convex-polygon))
  (%update-polygon-world-points polygon)
  (%update-polygon-bounding-box polygon))

(defmethod local-points ((polygon convex-polygon))
  (slot-value polygon 'local-points))

(defun %update-polygon-world-points (polygon)
  (declare (optimize (speed 3)))
  (with-slots ((rotation rotation)
               (w width) (h height)
               local-points local-center world-points)
      polygon
    (with-accessors ((world-x x) (world-y y) (world-z z)) (slot-value polygon 'world-position)
      (declare (world-position world-x world-y world-z)
               ((simple-array vector3) local-points world-points)
               (rotation-radians rotation))
      (loop :for local-point :across local-points
         :and world-point :across world-points :do
           (with-accessors ((local-x x) (local-y y)) local-point
             (declare (world-position local-x local-y))
             (setf (x world-point) local-x
                   (y world-point) local-y)
             (nrotate-2d world-point rotation local-center)
             (setf
              (x world-point) (the world-position
                                   (+ (the world-position (x world-point))
                                      world-x))
              (y world-point) (the world-position
                                   (+ (the world-position (y world-point))
                                      world-y))
              (z world-point) world-z))))
    world-points))

(defmethod world-points ((polygon convex-polygon))
  (slot-value polygon 'world-points))

(defun %initialize-polygon-width-height (polygon)
  (declare (type convex-polygon polygon))
  (loop with local-points = (local-points polygon)
     with right = (elt local-points 0)
     and down = (elt local-points 0)
     for point across local-points
     do (when (or (< (x point) 0)
                  (< (y point) 0))
          (error "polygon cannot have a negative local-point"))
       (when (> (x point) (x right))
         (setf right point))
       (when (> (y point) (y down))
         (setf down point))
     finally
       (setf (slot-value polygon 'width) (coerce (x right) 'world-dimension)
             (slot-value polygon 'height) (coerce (y down) 'world-dimension)
             (slot-value polygon 'local-center) (vector3
                                                 (/ (x right) 2)
                                                 (/ (y down) 2)
                                                 0.0)))
  polygon)

(defmethod (setf width) (new-width (polygon convex-polygon))
  (let* ((current-width (width polygon))
         (scaling-factor (/ new-width current-width)))
    (loop for local-point across (local-points polygon)do
         (setf (x local-point) (* (x local-point) scaling-factor)))
    (%initialize-polygon-width-height polygon)
    (%update-polygon-world-points polygon)
    (width polygon)))

(defmethod (setf height) (new-height (polygon convex-polygon))
  (let* ((current-height (height polygon))
         (scaling-factor (/ new-height current-height)))
    (loop for local-point across (local-points polygon)do
         (setf (y local-point) (* (y local-point) scaling-factor)))
    (%initialize-polygon-width-height polygon)
    (%update-polygon-world-points polygon)
    (height polygon)))

(defun %update-polygon-bounding-box (polygon)
  (declare (optimize (speed 3))
           (type convex-polygon polygon))
  (loop with world-points = (the (simple-array vector3) (world-points polygon))
     with left = (elt world-points 0)
     and right = (elt world-points 0)
     and up = (elt world-points 0)
     and down = (elt world-points 0)
     for point across world-points
     do
       (when (< (x point) (x left))
         (setf left point))
       (when (> (x point) (x right))
         (setf right point))
       (when (< (y point) (y up))
         (setf up point))
       (when (> (y point) (y down))
         (setf down point))
     finally
       (with-slots ((boundary bounding-box)) polygon
         (setf (x boundary) (x left)
               (y boundary) (y up)
               (z boundary) (z polygon)
               (width boundary) (ceiling (- (x right) (x left)))
               (height boundary) (ceiling (- (y down) (y up))))))
  polygon)

@inline
(defun %convex-polygon-collide (poly1 poly2)
  "Check POLY1 and POLY2 collision with separating axis theorem"
  (declare (optimize (speed 3)))
  (let ((poly1-points (world-points poly1))
        (poly2-points (world-points poly2)))
    (declare ((simple-array vector3) poly1-points poly2-points))
    (and (loop for p from 0 below (length poly1-points) do
              (unless (axis-project-overlap
                       (elt poly1-points p)
                       (elt poly1-points (if (= p (1- (length poly1-points)))
                                             0 (+ p 1)))
                       poly1-points
                       poly2-points)
                (return nil))
            finally (return T))
         (loop for p from 0 below (length poly2-points) do
              (unless (axis-project-overlap
                       (elt poly2-points p)
                       (elt poly2-points (if (= p (1- (length poly2-points)))
                                             0 (+ p 1)))
                       poly1-points
                       poly2-points)
                (return nil))
            finally (return T))
         (= (the world-position (z poly1))
            (the world-position (z poly2))))))

(defcollision ((polygon convex-polygon) (aabb aabb))
  (declare (optimize (speed 3)))
  (%convex-polygon-collide polygon aabb))
