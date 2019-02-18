(in-package :recurse.vert)

(defclass aabb (game-object)
  ((world-position :initform (make-point))
   (width :initarg :width
          :initform 1.0
          :type world-dimension
          :accessor width)
   (height :initarg :height
           :initform 1.0
           :type world-dimension
           :accessor height)
   (last-positions
    :type (vector world-position)
    :initform (make-array 3
                          :initial-element %uninitialized-interpolated-value%
                          :element-type 'world-position)
    :documentation "x-y-z positions of this object at the end of the last update frame. Used for interpolation.")
   (local-points :initform nil)
   (world-points :initform
                 (make-array
                  4
                  :initial-contents (list (make-point)
                                          (make-point)
                                          (make-point)
                                          (make-point))
                  :element-type 'point)))
  (:documentation "A(xis)A(ligned)B(ounding)B(ox)."))

(defmethod initialize-instance :after ((aabb aabb) &key (x 0) (y 0) (z 0))
  (with-slots (world-position width height) aabb
    (setf
     width (coerce width 'world-dimension)
     height (coerce height 'world-dimension)
     (point-x world-position) (coerce x 'world-position)
     (point-y world-position) (coerce y 'world-position)
     (point-z world-position) (coerce z 'world-position))))

(defgeneric bounding-box (game-object)
  (:documentation "Returns an AABB which surrounds the entire GAME-OBJECT")
  (:method ((aabb aabb)) aabb))

(defmethod x ((aabb aabb))
  (point-x (slot-value aabb 'world-position)))
(defmethod (setf x) (value (aabb aabb))
  (setf (point-x (slot-value aabb 'world-position)) (coerce value 'world-position)))
(defmethod y ((aabb aabb))
  (point-y (slot-value aabb 'world-position)))
(defmethod (setf y) (value (aabb aabb))
  (setf (point-y (slot-value aabb 'world-position)) (coerce value 'world-position)))
(defmethod z ((aabb aabb))
  (point-z (slot-value aabb 'world-position)))
(defmethod (setf z) (value (aabb aabb))
  (setf (point-z (slot-value aabb 'world-position)) (coerce value 'world-position)))

(defmethod rotation ((aabb aabb)) 0f0)

(defmethod interpolate-position ((game-object aabb) update-percent)
  (declare (optimize (speed 3)))
  (with-slots ((last last-positions) ) game-object
    (with-accessors ((x1 x) (y1 y) (z1 z)) game-object
      (declare (world-position x1 y1 z1)
               ((single-float 0.0 1.0) update-percent)
               ((simple-array world-position) last))
      (when (= %uninitialized-interpolated-value%
               (elt last 0))
        (setf (elt last 0) x1
              (elt last 1) y1
              (elt last 2) z1))
      ;; 0 = two update frames ago
      ;; 1 = last update frame
      (let* ((x0 (elt last 0))
             (y0 (elt last 1))
             (z0 (elt last 2))
             (ix (+ x0 (* update-percent (- x1 x0))))
             (iy (+ y0 (* update-percent (- y1 y0))))
             (iz (+ z0 (* update-percent (- z1 z0)))))
        (the (values world-position world-position)
             (values ix iy iz))))))

(defmethod pre-update ((aabb aabb))
  (with-slots ((last last-positions)) aabb
    (declare ((simple-array world-position) last))
    (setf (elt last 0) (x aabb)
          (elt last 1) (y aabb)
          (elt last 2) (z aabb)))
  (values))

(defmethod print-object ((aabb aabb) out)
  (print-unreadable-object (aabb out :type t)
    (format out "(~A,~A,~A)(~Ax~A)" (x aabb) (y aabb) (z aabb)
            (width aabb) (height aabb))))

(defmethod (setf width) :around (value (aabb aabb))
  (declare (real value))
  (call-next-method (coerce value 'world-dimension) aabb))

(defmethod (setf height) :around (value (aabb aabb))
  (declare (real value))
  (call-next-method (coerce value 'world-dimension) aabb))

(defgeneric local-points (aabb)
  (:documentation "Return a vector containing the four corners of the Bounding Box.
                   The points are relative to the Bounding Box's upper-left corner.
                   The order of the returned points is Upper-Left, Upper-Right, Lower-Right, Lower-Left.")
  (:method ((aabb aabb))
    (with-slots (local-points) aabb
      (unless local-points
        (with-slots ((w width) (h height)) aabb
          (setf local-points (make-array
                              4
                              :initial-contents (list (make-point :x 0 :y 0)
                                                      (make-point :x w :y 0)
                                                      (make-point :x w :y h)
                                                      (make-point :x 0 :y h))
                              :element-type 'point))))
      local-points)))

(defgeneric world-points (aabb)
  (:documentation "Return a vector containing the four corners of the Bounding Box.
                   The order of the returned points is Upper-Left, Upper-Right, Lower-Right, Lower-Left.")
  (:method ((aabb aabb))
    (declare (optimize (speed 3)
                       (space 3)))
    (with-accessors ((local-points local-points)) aabb
      (with-slots (world-points) aabb
        (with-slots ((world-x x) (world-y y) (world-z z)) (slot-value aabb 'world-position)
          (declare (world-position world-x world-y world-z)
                   ((simple-array point) local-points world-points))
          (map nil (lambda (local-point world-point)
                     (with-slots ((local-x x) (local-y y) (local-z z))
                         local-point
                       (declare (world-position local-x local-y local-z))
                       (setf
                        (slot-value world-point 'x) (+ local-x world-x)
                        (slot-value world-point 'y) (+ local-y world-y)
                        (slot-value world-point 'z) world-z)))
               local-points
               world-points))
        world-points))))

(declaim (inline %aabb-collision-check))
(defun %aabb-collision-check (rect1 rect2)
  (declare (optimize (speed 3))
           (aabb rect1 rect2))
  (with-slots ((p1 world-position)
               (w1 width) (h1 height))
      rect1
    (with-accessors ((x1 point-x) (y1 point-y) (z1 point-z))
        p1
      (with-slots ((p2 world-position)
                   (w2 width) (h2 height))
          rect2
        (with-accessors ((x2 point-x) (y2 point-y) (z2 point-z))
            p2
          (declare (world-dimension w1 h1 w2 h2)
                   (world-position x1 y1 z1 x2 y2 z2))
          (and (< x1 (+ x2 w2))
               (> (+ x1 w1) x2)
               ;; x1 falls inside rect2-x
               (< y1 (+ y2 h2))
               (> (+ h1 y1) y2)
               ;; y1 falls inside rect2-y
               ;; in the same 2d plane
               (= z1 z2)))))))


@export
@inline
(defun center-of (aabb)
  (declare (optimize (speed 3)
                     (space 3))
           (aabb aabb))
  (compute-center-of aabb (make-point)))

@export
@inline
(defun compute-center-of (aabb point)
  "Compute the center of AABB and update POINT to the result."
  (declare (optimize (speed 3)
                     (space 3))
           (point point)
           (aabb aabb))
  (setf
   (x point) (+ (the world-position (x aabb)) (/ (the world-dimension (width aabb)) 2.0))
   (y point) (+ (the world-position (y aabb)) (/ (the world-dimension (height aabb)) 2.0))
   (z point) (the world-position (z aabb)))
  point)

(defcollision ((rect1 aabb) (rect2 aabb))
  (declare (optimize (speed 3)))
  (%aabb-collision-check rect1 rect2))
