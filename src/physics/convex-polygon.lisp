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
                 (typep local-points '(array point))))
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
           :element-type 'point))
    (loop for i from 0 below (length local-points) do
         (setf (elt world-points i) (make-point))))
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
    (with-slots ((world-x x) (world-y y) (world-z z)) (slot-value polygon 'world-position)
      (declare (world-position world-x world-y world-z)
               ((simple-array point) local-points world-points)
               (rotation-radians rotation))
      (loop for local-point across local-points
         and world-point across world-points do
           (with-slots ((local-x x) (local-y y)) local-point
             (declare (world-position local-x local-y))
             (setf
              (slot-value world-point 'x) local-x
              (slot-value world-point 'y) local-y)
             (nrotate-2d world-point rotation local-center)
             (setf
              (slot-value world-point 'x) (the world-position
                                               (+ (the world-position (slot-value world-point 'x))
                                                  world-x))
              (slot-value world-point 'y) (the world-position
                                               (+ (the world-position (slot-value world-point 'y))
                                                  world-y))
              (slot-value world-point 'z) world-z))))
    world-points))

(defmethod world-points ((polygon convex-polygon))
  (slot-value polygon 'world-points))

(defun %initialize-polygon-width-height (polygon)
  (declare (type convex-polygon polygon))
  (loop with local-points = (local-points polygon)
     with right = (elt local-points 0)
     and down = (elt local-points 0)
     for point across local-points
     do (when (or (< (point-x point) 0)
                  (< (point-y point) 0))
          (error "polygon cannot have a negative local-point"))
       (when (> (point-x point) (point-x right))
         (setf right point))
       (when (> (point-y point) (point-y down))
         (setf down point))
     finally
       (setf (slot-value polygon 'width) (coerce (point-x right) 'world-dimension)
             (slot-value polygon 'height) (coerce (point-y down) 'world-dimension)
             (slot-value polygon 'local-center) (make-point
                                                 :x (/ (point-x right) 2)
                                                 :y (/ (point-y down) 2))))
  polygon)

(defmethod (setf width) (new-width (polygon convex-polygon))
  (let* ((current-width (width polygon))
         (scaling-factor (/ new-width current-width)))
    (loop for local-point across (local-points polygon)do
         (setf (point-x local-point) (* (point-x local-point) scaling-factor)))
    (%initialize-polygon-width-height polygon)
    (%update-polygon-world-points polygon)
    (width polygon)))

(defmethod (setf height) (new-height (polygon convex-polygon))
  (let* ((current-height (height polygon))
         (scaling-factor (/ new-height current-height)))
    (loop for local-point across (local-points polygon)do
         (setf (point-y local-point) (* (point-y local-point) scaling-factor)))
    (%initialize-polygon-width-height polygon)
    (%update-polygon-world-points polygon)
    (height polygon)))

(defun %update-polygon-bounding-box (polygon)
  (declare (optimize (speed 3)
                     (space 3))
           (type convex-polygon polygon))
  (loop with world-points = (the (simple-array point) (world-points polygon))
     with left = (elt world-points 0)
     and right = (elt world-points 0)
     and up = (elt world-points 0)
     and down = (elt world-points 0)
     for point across world-points
     do
       (when (< (point-x point) (point-x left))
         (setf left point))
       (when (> (point-x point) (point-x right))
         (setf right point))
       (when (< (point-y point) (point-y up))
         (setf up point))
       (when (> (point-y point) (point-y down))
         (setf down point))
     finally
       (with-slots ((boundary bounding-box)) polygon
         (setf (x boundary) (point-x left)
               (y boundary) (point-y up)
               (z boundary) (z polygon)
               (width boundary) (ceiling (- (point-x right) (point-x left)))
               (height boundary) (ceiling (- (point-y down) (point-y up))))))
  polygon)

@inline
(defun %convex-polygon-collide (poly1 poly2)
  "Check POLY1 and POLY2 collision with separating axis theorem"
  (declare (optimize (speed 3)))
  (let ((poly1-points (world-points poly1))
        (poly2-points (world-points poly2)))
    (declare ((simple-array point) poly1-points poly2-points))
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
  (flet ((custom-hit-box? (object)
           (not (eq object (hit-box object)))))
    (if (or (custom-hit-box? polygon) (custom-hit-box? aabb))
        (collidep (hit-box polygon) (hit-box aabb))
        (%convex-polygon-collide polygon aabb))))
