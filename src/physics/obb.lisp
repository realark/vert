(in-package :recurse.vert)

;;;; OBB base class

(defclass obb (transform)
  ((local-dimensions :initform (vector2 1.0 1.0)
                     :documentation "Local width and height.")
   (world-points-dirty-p :initform t)
   (world-points :initform nil
                 :documentation "The world-space points that define this OBB (including rotations).")
   (world-dimensions-dirty-p :initform t)
   (world-dimensions :initform (make-array 5
                                           :element-type 'single-float
                                           :initial-element 0.0)
                     :documentation "A bounding box around the OBB in world space.
This slot is used to cache the world AABB because it is frequently needed by collision and rendering code."))
  (:documentation "A 2D rectangle. X, Y, and Z are the rectangle's upper-left point. Rotation is around the rectangle's local center."))

(defun %mark-world-dimensions-dirty (obb)
  (setf (slot-value obb 'world-dimensions-dirty-p) t))

(defun %mark-world-points-dirty (obb)
  (setf (slot-value obb 'world-points-dirty-p) t))

(defmethod initialize-instance :after ((obb obb) &key (width 1.0) (height 1.0))
  (setf (width obb) width
        (height obb) height))

(defmethod print-object ((obb obb) out)
  (print-unreadable-object (obb out :type t)
    (format out
            "(~A,~A,~A)(~Ax~A)(r~A-degrees){~A}"
            (x obb) (y obb) (z obb)
            (width obb) (height obb)
            (rotation obb)
            (object-id obb))))

(defun world-points (obb)
  "Returns a vector of points which defines the OBB's shape in world space."
  (declare (optimize (speed 3))
           (obb obb))
  (with-slots (world-points world-points-dirty-p) obb
    (when world-points-dirty-p
      (unless world-points
        (setf world-points
              (make-array 4
                          :element-type 'vector3
                          :initial-contents (list
                                             (vector3)
                                             (vector3)
                                             (vector3)
                                             (vector3)))))
      (locally (declare ((simple-array vector3) world-points))
        (block fill-world-points-with-local-points
          ;; fill up the WORLD-POINTS vector with OBB's local points
          (with-accessors ((w width) (h height)) obb
            ;; upper-left
            (setf (x (elt world-points 0)) 0.0
                  (y (elt world-points 0)) 0.0
                  (z (elt world-points 0)) 0.0)
            ;; upper-right
            (setf (x (elt world-points 1)) w
                  (y (elt world-points 1)) 0.0
                  (z (elt world-points 1)) 0.0)
            ;; lower-right
            (setf (x (elt world-points 2)) w
                  (y (elt world-points 2)) h
                  (z (elt world-points 2)) 0.0)
            ;; lower-left
            (setf (x (elt world-points 3)) 0.0
                  (y (elt world-points 3)) h
                  (z (elt world-points 3)) 0.0)))
        (loop :for i :from 0 :below (length world-points) :do
             (let ((point (elt world-points i))
                   (transformed (transform-point (elt world-points i) obb)))
               (declare (dynamic-extent transformed))
               (copy-array-contents transformed point))))

      (setf world-points-dirty-p nil))
    world-points))

(defmethod width ((obb obb))
  (width (slot-value obb 'local-dimensions)))
(defmethod (setf width) (value (obb obb))
  (unless (eq value (width (slot-value obb 'local-dimensions)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (width (slot-value obb 'local-dimensions))))
      (unless (float= float-value current-value)
        (prog1 (setf (width (slot-value obb 'local-dimensions)) float-value)
          (%mark-world-points-dirty obb)
          (%mark-world-dimensions-dirty obb))))))

(defmethod height ((obb obb))
  (height (slot-value obb 'local-dimensions)))
(defmethod (setf height) (value (obb obb))
  (unless (eq value (height (slot-value obb 'local-dimensions)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (height (slot-value obb 'local-dimensions))))
      (unless (float= float-value current-value)
        (prog1 (setf (height (slot-value obb 'local-dimensions)) float-value)
          (%mark-world-points-dirty obb)
          (%mark-world-dimensions-dirty obb))))))

(defmethod mark-transform-dirty :after ((obb obb))
  (%mark-world-points-dirty obb)
  (%mark-world-dimensions-dirty obb))

@export
@inline
(defun center-of (obb)
  (declare (optimize (speed 3))
           (obb obb))
  (vector3 (+ (the world-position (x obb)) (/ (the world-dimension (width obb)) 2.0))
           (+ (the world-position (y obb)) (/ (the world-dimension (height obb)) 2.0))
           (the world-position (z obb))))

@export
(defun world-dimensions (obb)
  "Return bounding-box dimension info in base-world coordinates. (values x y z width height)
Note that for rotated objects, the dimensions represent an unrotated rectangle which the underlying object fits entirely within."
  ;; TODO: remove this function and use object-aabb directly
  (object-aabb obb))

@export
(defun object-aabb (object)
  "Dimensions of an AABB in world space which entirely contains OBJECT. XYZ are the AABB's upper-left corner. Returns: (values X Y Z W H)"
  (declare (optimize (speed 3))
           (obb object))
  (with-slots (world-dimensions world-dimensions-dirty-p) object
    (declare ((simple-array single-float (5)) world-dimensions)
             (boolean world-dimensions-dirty-p))
    (flet ((simple-obb-p (obb)
             (and
              (null (parent obb))
              (equalp 1.0 (the single-float (scale-x obb)))
              (equalp 1.0 (the single-float (scale-y obb)))
              (equalp 0.0 (the single-float (rotation obb)))))
           (simple-obb-dimensions (obb)
             (values (x obb)
                     (y obb)
                     (z obb)
                     (width obb)
                     (height obb)))
           (complex-obb-dimensions (obb)
             (let* ((tmp (vector3 0f0 0f0 0f0))
                    (world-point (transform-point tmp obb))
                    (world-width (* (the single-float (scale-x obb))
                                    (the single-float (width obb))))
                    (world-height (* (the single-float (scale-y obb))
                                     (the single-float (height obb))))
                    (world-rotation (rotation obb)))
               (declare (dynamic-extent world-point tmp)
                        (single-float world-width world-height)
                        (rotation-radians world-rotation))
               (loop :with parent = (parent obb) :while parent :do
                    (setf world-width (* world-width
                                         (the single-float (scale-x parent)))
                          world-height (* world-height
                                          (the single-float (scale-y parent)))
                          world-rotation (if (= 0.0 (the single-float (rotation parent)))
                                             world-rotation
                                             (mod (+ world-rotation
                                                     (the single-float (rotation parent)))
                                                  tau))
                          parent (parent parent)))
               (if (float= 0.0 world-rotation)
                   (values (x world-point)
                           (y world-point)
                           (z world-point)
                           world-width
                           world-height)
                   ;; object has a rotation, return an unrotated rectangle which fits over the object
                   ;; TODO: make this the default path if profiling show no performance hit
                   (loop :with min-x = (the single-float (x world-point))
                      :and max-x = (the single-float (x world-point))
                      :and max-y = (the single-float (y world-point))
                      :and min-y = (the single-float (y world-point))
                      :for point :across (the (simple-array vector3) (world-points obb)) :do
                        (with-accessors ((point-x x) (point-y y)) point
                          (declare (single-float point-x point-y))
                          (when (< point-x min-x)
                            (setf min-x point-x))
                          (when (> point-x max-x)
                            (setf max-x point-x))
                          (when (< point-y min-y)
                            (setf min-y point-y))
                          (when (> point-y max-y)
                            (setf max-y point-y)))
                      :finally
                        (return
                          (values min-x
                                  min-y
                                  (z world-point)
                                  (- max-x min-x)
                                  (- max-y min-y))))))))
      (declare (inline simple-obb-p simple-obb-dimensions complex-obb-dimensions))
      (when world-dimensions-dirty-p
        (multiple-value-bind (x y z w h)
            (if (simple-obb-p object)
                (simple-obb-dimensions object)
                (complex-obb-dimensions object))
          (setf (elt world-dimensions 0) x
                (elt world-dimensions 1) y
                (elt world-dimensions 2) z
                (elt world-dimensions 3) w
                (elt world-dimensions 4) h
                world-dimensions-dirty-p nil)))
      (values (elt world-dimensions 0)
              (elt world-dimensions 1)
              (elt world-dimensions 2)
              (elt world-dimensions 3)
              (elt world-dimensions 4)))))

(defun aabb-collidep (rect1 rect2)
  "T if RECT1 and RECT2's world-space AABBs collide."
  (declare (optimize (speed 3))
           (obb rect1 rect2))
  (multiple-value-bind (x1 y1 z1 w1 h1)
      (world-dimensions rect1)
    (multiple-value-bind (x2 y2 z2 w2 h2)
        (world-dimensions rect2)
      (declare (world-dimension w1 h1 w2 h2)
               (world-position x1 y1 z1 x2 y2 z2))
      (and (< x1 (+ x2 w2))
           (> (+ x1 w1) x2)
           ;; x1 falls inside rect2-x
           (< y1 (+ y2 h2))
           (> (+ h1 y1) y2)
           ;; y1 falls inside rect2-y
           ;; in the same 2d plane
           (= z1 z2)))))

(defun convex-poly-collidep (poly1 poly2)
  "Assume POLY1 and POLY2 are convex polys and return t if they collide."
  (declare (optimize (speed 3))
           (obb poly1 poly2))
  ;; check for collision using separating axis theorem
  (let ((poly1-points (world-points poly1))
        (poly2-points (world-points poly2)))
    (declare ((simple-array vector3) poly1-points poly2-points))
    (and (loop :for p :from 0 :below (length poly1-points) :do
              (unless (axis-project-overlap
                       (elt poly1-points p)
                       (elt poly1-points (if (= p (1- (length poly1-points)))
                                             0 (+ p 1)))
                       poly1-points
                       poly2-points)
                (return nil))
            finally (return T))
         (loop :for p :from 0 :below (length poly2-points) :do
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

(defcollision ((rect1 obb) (rect2 obb))
  (declare (optimize (speed 3)))
  (and
   ;; first ensure world bounding-boxes overlap
   (aabb-collidep rect1 rect2)
   (if (and (eq (parent rect1) (parent rect2))
            (= 0.0
               (the single-float (rotation rect1))
               (the single-float (rotation rect2))))
       ;; unrotated rects in world space. No more checking required.
       t
       ;; expensive checking with separating axis theorem
       (convex-poly-collidep rect1 rect2))))

;;;; util OBB classes

@export-class
(defclass cross (obb)
  ((vertical-rect :initform nil)
   (horizontal-rect :initform nil))
  (:documentation "A cross-shaped hitbox which favors different axes for the two sections. Horizontal favors X and vertical favors Y.
Useful to simulate an agent with feet pushing down on the ground and/or hands pushing to the side of a wall."))

(defmethod initialize-instance :after ((cross cross) &key (vertical-width 1) (horizontal-height 1))
  (with-slots (vertical-rect horizontal-rect) cross
    (with-accessors ((width width) (height height) (x x) (y y) (z z)) cross
      (setf vertical-rect (make-instance 'obb
                                         :width vertical-width
                                         :height height
                                         :x (- (/ width 2) (/ vertical-width 2))
                                         :y 0
                                         :z 0)
            horizontal-rect (make-instance 'obb
                           :width width
                           :height horizontal-height
                           :x 0
                           :y (- (/ height 2) (/ horizontal-height 2))
                           :z 0))
      (setf (parent vertical-rect) cross
            (parent horizontal-rect) cross)
      (values))))

(defmethod favored-collision-resolution-axis ((cross cross) stationary-object)
    (with-slots (vertical-rect horizontal-rect) cross
      (cond ((collidep vertical-rect stationary-object) 'x)
            ((collidep horizontal-rect stationary-object) 'y)
            (t 'x))))

@export-class
(defclass static-obb (obb static-object)
  ())
