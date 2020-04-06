(in-package :recurse.vert)

;;;; OBB base class

(defclass obb (transform)
  ((local-dimensions :initform (vector2 1.0 1.0)
                     :documentation "Local width and height.")
   (world-points :initform nil)
   (local-points :initform nil)
   (world-dimensions-dirty-p :initform t)
   (world-dimensions :initform (make-array 5
                                           :element-type 'single-float
                                           :initial-element 0.0)))
  (:documentation "A 2D rectangle. X, Y, and Z are the rectangle's upper-left point. Rotation is around the rectangle's local center."))

(defun %%mark-world-dimensions-dirty (obb)
  (setf (slot-value obb 'world-dimensions-dirty-p) t))

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

(defun %set-local-points (obb)
  (declare (optimize (speed 3))
           (obb obb))
  (with-slots (local-points) obb
    (with-accessors ((w width) (h height)) obb
      (if local-points
          (locally (declare ((simple-array vector3) local-points))
            (setf (x (elt local-points 1)) w
                  (x (elt local-points 2)) w
                  (y (elt local-points 2)) h
                  (y (elt local-points 3)) h))
        (setf local-points (make-array
                            4
                            :element-type 'vector3
                            :initial-contents (list (vector3 0.0 0.0 0.0)
                                                    (vector3 w 0.0 0.0)
                                                    (vector3 w h 0.0)
                                                    (vector3 0.0 h 0.0))))))))

(defgeneric local-points (obb)
  (:documentation "Return a vector containing the four corners of the Bounding Box.
                   The points are relative to the Bounding Box's upper-left corner.
                   The order of the returned points is Upper-Left, Upper-Right, Lower-Right, Lower-Left.")
  (:method ((obb obb))
    (with-slots (local-points) obb
      (unless local-points
        (%set-local-points obb))
      local-points)))

(defmethod width ((obb obb))
  (width (slot-value obb 'local-dimensions)))
(defmethod (setf width) (value (obb obb))
  (unless (eq value (width (slot-value obb 'local-dimensions)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (width (slot-value obb 'local-dimensions))))
      (unless (float= float-value current-value)
        (prog1 (setf (width (slot-value obb 'local-dimensions)) float-value)
          (%set-local-points obb)
          (%mark-dirty obb))))))

(defmethod height ((obb obb))
  (height (slot-value obb 'local-dimensions)))
(defmethod (setf height) (value (obb obb))
  (unless (eq value (height (slot-value obb 'local-dimensions)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (height (slot-value obb 'local-dimensions))))
      (unless (float= float-value current-value)
        (prog1 (setf (height (slot-value obb 'local-dimensions)) float-value)
          (%set-local-points obb)
          (%mark-dirty obb))))))

;; TODO: could optimize further by only computing world points when the transform is dirtied
(defun world-points (obb)
  (declare (optimize (speed 3))
           (obb obb))
  (with-accessors ((local-points local-points)) obb
    (declare ((simple-array vector3) local-points))
    (with-slots (world-points) obb
      (unless world-points
        (setf world-points
              (make-array (length local-points)
                          :element-type 'vector3
                          :initial-element (vector3 0.0 0.0 0.0)))
        (locally (declare ((simple-array vector3) world-points))
          (loop :for i :from 1 :below (length world-points) :do
               (setf (elt world-points i) (vector3 0.0 0.0 0.0)))))
      (locally (declare ((simple-array vector3) world-points))
        (loop :for i :from 0 :below (length local-points) :do
             (let ((point (elt world-points i))
                   (transformed (transform-point (elt local-points i) obb)))
               (declare (dynamic-extent transformed))
               (copy-array-contents transformed point))))
      world-points)))

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
  (declare (optimize (speed 3))
           (obb obb))
  (with-slots (world-dimensions world-dimensions-dirty-p) obb
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
            (if (simple-obb-p obb)
                (simple-obb-dimensions obb)
                (complex-obb-dimensions obb))
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
  "Assume RECT1 and RECT2 are AABBs in the same space and return t if they collide."
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

(defun world-points-collidep (rect1 rect2)
  "Check if the two unrotated rects collide by using their world-points."
  (declare (optimize (speed 3)))
  (convex-poly-collidep rect1 rect2)
  (let ((points1 (world-points rect1))
        (points2 (world-points rect2)))
    (declare ((simple-array vector3 (4)) points1 points2))
    (let ((left1 (x (elt points1 0)))
          (right1 (x (elt points1 1)))
          (top1 (y (elt points1 0)))
          (bottom1 (y (elt points1 3)))
          (left2 (x (elt points2 0)))
          (right2 (x (elt points2 1)))
          (top2 (y (elt points2 0)))
          (bottom2 (y (elt points2 3))))
      (declare (single-float left1 right1 top1 bottom1
                             left2 right2 top2 bottom2))
      (and (< left1 right2)
           (> right1 left2)
           ;; x1 falls inside rect2-x
           (< top1 bottom2)
           (> bottom1 top2)
           ;; y1 falls inside rect2-y
           ;; in the same 2d plane
           (float= (z rect1) (z rect2))))))

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
  (if (and (float= 0f0
                   (the single-float (rotation rect1)))
           (float= 0f0
                   (the single-float (rotation rect2))))
      (if (eq (parent rect1) (parent rect2))
          (aabb-collidep rect1 rect2)
          (world-points-collidep rect1 rect2))
      (convex-poly-collidep rect1 rect2)))

(defmethod (setf x) :after (value (obb obb))
  (%%mark-world-dimensions-dirty obb))
(defmethod (setf y) :after (value (obb obb))
  (%%mark-world-dimensions-dirty obb))
(defmethod (setf z) :after (value (obb obb))
  (%%mark-world-dimensions-dirty obb))
(defmethod (setf width) :after (value (obb obb))
  (%%mark-world-dimensions-dirty obb))
(defmethod (setf height) :after (value (obb obb))
  (%%mark-world-dimensions-dirty obb))

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
