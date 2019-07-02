(in-package :recurse.vert)

(defclass obb (transform)
  ((local-dimensions :initform (vector2 1.0 1.0)
                     :documentation "Local width and height.")
   (world-points :initform nil)
   (local-points :initform nil))
  (:documentation "A 2D rectangle which may be rotated about its local center."))

(defmethod initialize-instance :after ((obb obb) &key (width 1.0) (height 1.0))
  (setf (width obb) width
        (height obb) height))

(defmethod print-object ((obb obb) out)
  (print-unreadable-object (obb out :type t)
    (format out
            "(~A,~A,~A)(~Ax~A)(r~A-degrees)"
            (x obb) (y obb) (z obb)
            (width obb) (height obb)
            (rotation obb))))

(defmethod width ((obb obb))
  (width (slot-value obb 'local-dimensions)))
(defmethod (setf width) (value (obb obb))
  (setf (width (slot-value obb 'local-dimensions)) (coerce value 'single-float)))

(defmethod height ((obb obb))
  (height (slot-value obb 'local-dimensions)))
(defmethod (setf height) (value (obb obb))
  (setf (height (slot-value obb 'local-dimensions)) (coerce value 'single-float)))

(defgeneric local-points (obb)
  (:documentation "Return a vector containing the four corners of the Bounding Box.
                   The points are relative to the Bounding Box's upper-left corner.
                   The order of the returned points is Upper-Left, Upper-Right, Lower-Right, Lower-Left.")
  (:method ((obb obb))
    (with-slots (local-points) obb
      (unless local-points
        (with-accessors ((w width) (h height)) obb
          (setf local-points (make-array
                              4
                              :element-type 'vector3
                              :initial-contents (list (vector3 0.0 0.0 0.0)
                                                      (vector3 w 0.0 0.0)
                                                      (vector3 w h 0.0)
                                                      (vector3 0.0 h 0.0))))))
      local-points)))

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

@inline
(defun %world-dimensions (obb)
  "Return OBB's (values x y z w h) in world space."
  (declare (optimize (speed 3))
           (obb obb))
  (if (and (null (parent obb))
           (= 1.0 (scale-x obb) (scale-y obb)))
      (values (x obb)
              (y obb)
              (z obb)
              (width obb)
              (height obb))
      (let ((world-position (elt (the (simple-array vector3) (world-points obb)) 0))
            (world-dimension (elt (the (simple-array vector3) (world-points obb)) 2)))
        (values (x world-position)
                (y world-position)
                (z world-position)
                (- (width world-dimension) (x world-position))
                (- (height world-dimension) (y world-position))))))

(defun aabb-collidep (rect1 rect2)
  "Assume RECT1 and RECT2 are AABBs in the same space and return t if they collide."
  (declare (optimize (speed 3))
           (obb rect1 rect2))
  (multiple-value-bind (x1 y1 z1 w1 h1)
      (%world-dimensions rect1)
    (multiple-value-bind (x2 y2 z2 w2 h2)
        (%world-dimensions rect2)
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
  (if (= 0f0
         (the single-float (rotation rect1))
         (the single-float (rotation rect2)))
      (aabb-collidep rect1 rect2)
      (convex-poly-collidep rect1 rect2)))

@export-class
(defclass cross (obb)
  ((vertical-rect :initform nil)
   (horizontal-rect :initform nil))
  (:documentation "A cross-shaped hitbox which favors different axes for the two sections. Horizontal favors X and vertical favors Y."))

(defmethod initialize-instance :after ((cross cross) &key (vertical-width 1) (horizontal-height 1))
  (with-slots (vertical-rect horizontal-rect) cross
    (with-accessors ((width width) (height height) (x x) (y y) (z z)) cross
      (setf vertical-rect (make-instance 'obb
                                         :width vertical-width
                                         :height height
                                         :x (/ width 2)
                                         :y 0
                                         :z 0)
            horizontal-rect (make-instance 'obb
                           :width width
                           :height horizontal-height
                           :x 0
                           :y (/ height 2)
                           :z 0))
      (setf (parent vertical-rect) cross
            (parent horizontal-rect) cross))))

;; TODO instead of requiring a custom collision check hit-box could be extended to allow for multiple hit boxes
(defcollision ((cross cross) (rect obb))
  (declare (optimize (speed 3)))
  (with-slots (vertical-rect horizontal-rect) cross
    (or (collidep vertical-rect rect)
        (collidep horizontal-rect rect))))

(defmethod favored-collision-resolution-axis ((cross cross) stationary-object)
    (with-slots (vertical-rect horizontal-rect) cross
      (cond ((collidep vertical-rect stationary-object) 'x)
            ((collidep horizontal-rect stationary-object) 'y))))
