;;;; Utils for common math operations

(in-package :recurse.vert)

(defun deg->rad (x)
  ;; (declare (optimize (speed 3)))
  (declare (rotation-degrees x))
  (the rotation-radians (* x #.(/ float-pi 180))))

(defun rad->deg (x)
  ;; (declare (optimize (speed 3)))
  (declare (rotation-radians x))
  (the rotation-degrees (* x #.(/ 180 float-pi))))

(defun rotate-2d (point theta &optional (rotation-origin *origin*))
  "Rotate POINT THETA radians about ROTATION-ORIGIN in the xy plane."
  (declare (type vector3 point rotation-origin))
  (let ((copy (vector3 0.0 0.0 0.0)))
    (copy-array-contents point copy)
    (nrotate-2d copy
                theta
                rotation-origin)))

(defun nrotate-2d (point theta &optional (rotation-origin *origin*))
  "Destructively rotate POINT THETA radians about ROTATION-ORIGIN in the xy plane."
  (declare (optimize (speed 3))
           (rotation-radians theta)
           (vector3 point rotation-origin))
  ;; translate point to rotation-origin
  (let ((cos-theta (cos theta))
        (sin-theta (sin theta)))
    (with-accessors ((x x) (y y)) point
      (with-accessors ((origin-x x) (origin-y y)) rotation-origin
        (declare (world-position x y origin-x origin-y))
        ;; translate to origin
        (setf x (- x origin-x)
              y (- y origin-y))
        ;; rotation equation looks a bit different because
        ;; y axis increases in the downward direction
        (let ((rotated-x (coerce (+ (* x cos-theta) (* y sin-theta)) 'world-position))
              (rotated-y (coerce (- (* y cos-theta) (* x sin-theta)) 'world-position)))
          ;; rotate
          (setf x rotated-x
                y rotated-y))
        ;; translate back from origin
        (setf x (+ x origin-x)
              y (+ y origin-y))
        point))))

@inline
(defun line-segment-point-closest-to (a b c)
  "Constructs a point on the line segment between points A and B
   which is closest to point C."
  (declare (type vector3 a b c))
  ;; k = [(Cx-Ax)(Bx-Ax)+(Cy-Ay)(By-Ay)] / [(Bx-Ax)^2+(By-Ay)^2]
  ;; Dx=Ax+k(Bx-Ax)
  ;; Dy=Ay+k(By-Ay)
  (with-accessors ((a-x x) (a-y y)) a
    (with-accessors ((b-x x) (b-y y)) b
      (with-accessors ((c-x x) (c-y y)) c
        (let* ((k (/ (+ (* (- c-x a-x) (- b-x a-x))
                        (* (- c-y a-y) (- b-y a-y)))
                     (+ (expt (- b-x a-x) 2)
                        (expt (- b-y a-y) 2))))
               (d-x (+ a-x (* k (- b-x a-x))))
               (d-y (+ a-y (* k (- b-y a-y)))))
          (if (or (and (<= (float a-x) d-x (float b-x)) (<= (float a-y) d-y (float b-y)))
                  (and (<= (float b-x) d-x (float a-x)) (<= (float b-y) d-y (float a-y))))
              (vector3 d-x d-y (z a))
              (if (< (distance-between a c) (distance-between b c))
                  (copy-vector a)
                  (copy-vector b))))))))

(defun distance-along-axis (axis point)
  "Project POINT onto AXIS and compute a scalar value."
  (declare (optimize (speed 3))
           (vector3 point)
           (vector2 axis))
  (with-accessors ((point-x x) (point-y y)) point
    (with-accessors ((axis-x x) (axis-y y)) axis
      (declare (world-position axis-x axis-y))
      (let* ((tmp (/
                   (+ (* point-x axis-x) (* point-y axis-y))
                   (+ (expt axis-x 2) (expt axis-y 2))))
             (proj-x (* tmp axis-x))
             (proj-y (* tmp axis-y)))
        (+ (* proj-x axis-x) (* proj-y axis-y))))))

@inline
(defun project-onto-axis (axis point)
  "Project POINT onto AXIS and compute a scalar value."
  (declare (optimize (speed 3))
           (type point point)
           (type vector2 axis))
  (with-accessors ((point-x x) (point-y y)) point
    (with-slots ((axis-x x) (axis-y y)) axis
      (declare (world-position axis-x axis-y))
      (let* ((tmp (/
                   (+ (* point-x axis-x) (* point-y axis-y))
                   (+ (expt axis-x 2) (expt axis-y 2))))
             (proj-x (* tmp axis-x))
             (proj-y (* tmp axis-y)))
        (+ (* proj-x axis-x) (* proj-y axis-y))))))

(defun same-sinage (x1 x2)
  (or (and (< x1 0) (< x2 0))
      (and (>= x1 0) (>= x2 0))))

(defun axis-project-overlap (p1 p2 polygon1 polygon2)
  "T if the points in POLYGON1 and POLYGON2 overlap once projected onto the
   face normal of the face between P1 and P2."
  (declare (optimize (speed 3))
           (vector3 p1 p2)
           ((simple-array vector3) polygon1 polygon2))
  (let ((poly1-max nil) (poly1-min nil)
        (poly2-max nil) (poly2-min nil)
        (axis (vector2 (- (x p1) (x p2))
                       (- (y p1) (y p2)))))
    (declare (dynamic-extent axis))
    (loop :for point :across polygon1 :do
         (let ((distance (the world-position (distance-along-axis axis point))))
           (when (or (null poly1-max) (> distance poly1-max))
             (setf poly1-max distance))
           (when (or (null poly1-min) (< distance poly1-min))
             (setf poly1-min distance))))
    (loop :for point :across polygon2 :do
         (let ((distance (the world-position (distance-along-axis axis point))))
           (when (or (null poly2-max) (> distance poly2-max))
             (setf poly2-max distance))
           (when (or (null poly2-min) (< distance poly2-min))
             (setf poly2-min distance))))
    (and (< poly2-min poly1-max) (>= poly2-max poly1-min))))

;;;; Non-consing matrix utils
;; Matrix consing operations are declared inlined and can be used with declare dynamic-extent for non-consing matrix operations

(deftype matrix ()
  "4x4 matrix"
  `(simple-array single-float (16)))

@inline
(defun matrix (m11 m12 m13 m14
               m21 m22 m23 m24
               m31 m32 m33 m34
               m41 m42 m43 m44)
  "Construct MATRIX with the given elements (in provided in row major order.)"
  (make-array 16
              :element-type 'single-float
              :initial-contents (list m11 m21 m31 m41
                                      m12 m22 m32 m42
                                      m13 m23 m33 m43
                                      m14 m24 m34 m44)))

(defun copy-array-contents (src-array dest-array)
  "Copy contents of SRC-ARRAY to DEST-ARRAY and return the dest array."
  (declare (simple-array src-array dest-array))
  (assert (= (length src-array) (length dest-array)))
  (loop :for i :from 0 :below (length src-array) :do
       (setf (elt dest-array i) (elt src-array i)))
  dest-array)

@inline
(defun identity-matrix ()
  "Construct an identity matrix."
  (matrix 1f0 0f0 0f0 0f0
          0f0 1f0 0f0 0f0
          0f0 0f0 1f0 0f0
          0f0 0f0 0f0 1f0))

@inline
(defun translation-matrix (x y z)
  "Construct a translation matrix from X Y Z"
  (declare (optimize (speed 3))
           (single-float x y z))
  (matrix 1f0 0f0 0f0 x
          0f0 1f0 0f0 y
          0f0 0f0 1f0 z
          0f0 0f0 0f0 1f0))

@inline
(defun matrix* (&rest matrices)
  (declare (optimize (speed 3))
           (dynamic-extent matrices))
  (let ((product-matrix (identity-matrix))
        (tmp (identity-matrix)))
    (declare (matrix product-matrix tmp)
             (dynamic-extent tmp))
    (labels ((mref (matrix row column)
               (declare (matrix matrix))
               (aref matrix (+ row (* column 4))))
             ((setf mref) (value matrix row column)
               (declare (matrix matrix))
               (setf (aref matrix (+ row (* column 4))) value)))
      (macrolet ((inline-mul (left right dest)
                   `(progn
                      ,@(loop :for i :below 4
                           :append (loop :for j :below 4
                                      :collect
                                        `(setf
                                          (mref ,dest ,i ,j)
                                          (+ ,@(loop :for k :below 4
                                                  :collect `(* (mref ,left ,i ,k) (mref ,right ,k ,j))))))))))
        (loop :for matrix :in matrices :do
             (locally (declare ((simple-array single-float (16)) matrix))
               (loop :for i :from 0 :below (length tmp) :do
                    (setf (elt tmp i) 0.0))
               (inline-mul product-matrix matrix tmp)
               (loop :for i :from 0 :below (length tmp) :do
                    (setf (elt product-matrix i) (elt tmp i)))))))
    product-matrix))

@inline
(defun rotation-matrix (x y z)
  "Construct a rotation matrix from rotation factors X Y Z"
  (declare (optimize (speed 3))
           (single-float x y z))
  (let ((result (identity-matrix)))
    (declare (matrix result))
    (unless (= 0f0 z)
      (let* ((c (cos z))
             (s (sin z))
             (z-rotation (matrix
                          c     (- s) 0f0    0f0
                          s     c     0f0    0f0
                          0f0   0f0   1f0    0f0
                          0f0   0f0   0f0    1f0))
             (product (matrix* result z-rotation)))
        (declare (dynamic-extent z-rotation product))
        (copy-array-contents product result)))
    (unless (= 0f0 y)
      (let* ((c (cos y))
             (s (sin y))
             (y-rotation (matrix
                          c     0f0   s      0f0
                          0f0   1f0   0f0    0f0
                          (- s) 0f0   c      0f0
                          0f0   0f0   0f0    1f0))
             (product (matrix* result y-rotation)))
        (declare (dynamic-extent y-rotation product))
        (copy-array-contents product result)))
    (unless (= 0f0 x)
      (let* ((c (cos x))
             (s (sin x))
             (x-rotation (matrix
                          1f0   0f0   0f0    0f0
                          0f0   c     (- s)  0f0
                          0f0   s     c      0f0
                          0f0   0f0   0f0    1f0))
             (product (matrix* result x-rotation)))
        (declare (dynamic-extent x-rotation product))
        (copy-array-contents product result)))
    result))

@inline
(defun scale-matrix (w h d)
  "Construct a scale matrix from W H D scale factors."
  (declare (optimize (speed 3))
           (single-float w h d))
  (matrix
   w    0f0  0f0  0f0
   0f0  h    0f0  0f0
   0f0  0f0  d    0f0
   0f0  0f0  0f0  1f0))

@inline
(defun ortho-matrix (left right bottom top near far)
  "Construct an orthographic projection matrix"
  (declare (optimize (speed 3))
           (world-position left right bottom top near far))
  (let ((r-l (- right left))
        (t-b (- top bottom))
        (f-n (- far near)))
    (matrix (/ 2.0 r-l) 0.0 0.0 (- (/ (+ right left) r-l))
            0.0 (/ 2.0 t-b) 0.0 (- (/ (+ top bottom) t-b))
            0.0 0.0 (/ -2.0 f-n) (- (/ (+ far near) f-n))
            0.0 0.0 0.0 1.0)))

#+sbcl
(eval-when (:compile-toplevel)
  (setf sb-ext:*inline-expansion-limit* 1000))

@inline
(defun inverse-matrix (matrix)
  (declare (optimize (speed 3))
           (matrix matrix))
  (let ((det (sb-cga:matrix-determinant matrix)))
    (if (< (abs det) sb-cga:+default-epsilon+)
        (error "Cannot invert matrix with zero determinant:~%  ~S"
               matrix)
        (macrolet ((a (x y z)
                     (multiple-value-bind (r1 c1) (truncate (- x 11) 10)
                       (multiple-value-bind (r2 c2) (truncate (- y 11) 10)
                         (multiple-value-bind (r3 c3) (truncate (- z 11) 10)
                           `(* (sb-cga:mref matrix ,r1 ,c1)
                               (sb-cga:mref matrix ,r2 ,c2)
                               (sb-cga:mref matrix ,r3 ,c3)))))))
          (let ((m
                 (matrix
                  ;; row 1
                  (- (+ (a 22 33 44) (a 23 34 42) (a 24 32 43))
                     (a 22 34 43) (a 23 32 44) (a 24 33 42))
                  (- (+ (a 12 34 43) (a 13 32 44) (a 14 33 42))
                     (a 12 33 44) (a 13 34 42) (a 14 32 43))
                  (- (+ (a 12 23 44) (a 13 24 42) (a 14 22 43))
                     (a 12 24 43) (a 13 22 44) (a 14 23 42))
                  (- (+ (a 12 24 33) (a 13 22 34) (a 14 23 32))
                     (a 12 23 34) (a 13 24 32) (a 14 22 33))
                  ;; row 2
                  (- (+ (a 21 34 43) (a 23 31 44) (a 24 33 41))
                     (a 21 33 44) (a 23 34 41) (a 24 31 43))
                  (- (+ (a 11 33 44) (a 13 34 41) (a 14 31 43))
                     (a 11 34 43) (a 13 31 44) (a 14 33 41))
                  (- (+ (a 11 24 43) (a 13 21 44) (a 14 23 41))
                     (a 11 23 44) (a 13 24 41) (a 14 21 43))
                  (- (+ (a 11 23 34) (a 13 24 31) (a 14 21 33))
                     (a 11 24 33) (a 13 21 34) (a 14 23 31))
                  ;; row 3
                  (- (+ (a 21 32 44) (a 22 34 41) (a 24 31 42))
                     (a 21 34 42) (a 22 31 44) (a 24 32 41))
                  (- (+ (a 11 34 42) (a 12 31 44) (a 14 32 41))
                     (a 11 32 44) (a 12 34 41) (a 14 31 42))
                  (- (+ (a 11 22 44) (a 12 24 41) (a 14 21 42))
                     (a 11 24 42) (a 12 21 44) (a 14 22 41))
                  (- (+ (a 11 24 32) (a 12 21 34) (a 14 22 31))
                     (a 11 22 34) (a 12 24 31) (a 14 21 32))
                  ;; row 4
                  (- (+ (a 21 33 42) (a 22 31 43) (a 23 32 41))
                     (a 21 32 43) (a 22 33 41) (a 23 31 42))
                  (- (+ (a 11 32 43) (a 12 33 41) (a 13 31 42))
                     (a 11 33 42) (a 12 31 43) (a 13 32 41))
                  (- (+ (a 11 23 42) (a 12 21 43) (a 13 22 41))
                     (a 11 22 43) (a 12 23 41) (a 13 21 42))
                  (- (+ (a 11 22 33) (a 12 23 31) (a 13 21 32))
                     (a 11 23 32) (a 12 21 33) (a 13 22 31)))))
            (dotimes (i 4)
              (dotimes (j 4)
                (setf (sb-cga:mref m i j) (/ (sb-cga:mref m i j) det))))
            m)))))

@inline
(defun interpolate-matrix (m0 m1 interpolation-value)
  "Construct an interpolation matrix between M0 and M1 based on the INTERPOLATION-VALUE. 0.0 = m0, 1.0 = m1."
  (declare (optimize (speed 3))
           (matrix m0 m1)
           ((single-float 0.0 1.0) interpolation-value))
  (let ((interpolated-matrix  (identity-matrix)))
    (loop :for i :from 0 :below (length m0) :do
         (setf (elt interpolated-matrix i)
               (+ (* (- 1.0 interpolation-value) (elt m0 i))
                  (* interpolation-value (elt m1 i)))))
    interpolated-matrix))

@export
(defun distance-between-objects (obj1 obj2)
  "Compute the distance between the centers of OBJ1 and OBJ2"
  (declare (optimize (speed 3))
           (transform obj1 obj2))
  (let* ((obj1-center (vector2 (/ (the single-float (width obj1)) 2.0)
                               (/ (the single-float (height obj1)) 2.0)))
         (obj2-center (vector2 (/ (the single-float (width obj2)) 2.0)
                               (/ (the single-float (height obj2)) 2.0)))
         (transformed-obj1-center (transform-point obj1-center obj1 obj2)))
    (declare (dynamic-extent obj1-center obj2-center transformed-obj1-center))
    (with-accessors ((x1 x) (y1 y)) transformed-obj1-center
      (with-accessors ((x2 x) (y2 y)) obj2-center
        (declare (single-float x1 y1 x2 y2))
        (the single-float
             (sqrt (+ (expt (- x1 x2) 2)
                      (expt (- y1 y2) 2))))))))
