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
  (declare (type point point rotation-origin))
  (nrotate-2d (copy-point point)
              theta
              rotation-origin))

(defun nrotate-2d (point theta &optional (rotation-origin *origin*))
  "Destructively rotate POINT THETA radians about ROTATION-ORIGIN in the xy plane."
  (declare (optimize (speed 3))
           (rotation-radians theta)
           (point point rotation-origin))
  ;; translate point to rotation-origin
  (let ((cos-theta (cos theta))
        (sin-theta (sin theta)))
    (with-accessors ((x point-x) (y point-y)) point
      (with-accessors ((origin-x point-x) (origin-y point-y)) rotation-origin
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

(defun line-segment-point-closest-to (a b c)
  "Return the point on the line segment between points A and B
   which is closest to point C."
  (declare (type point a b c))
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
              (make-point :x d-x :y d-y :z (z a))
              (if (< (distance-between a c) (distance-between b c))
                  a b)))))))

(defun distance-along-axis (axis point)
  "Project POINT onto AXIS and compute a scalar value."
  (declare (optimize (speed 3))
           (point point)
           (vector2 axis))
  (with-accessors ((point-x point-x) (point-y point-y)) point
    (with-slots ((axis-x x) (axis-y y)) axis
      (declare (world-position axis-x axis-y))
      (let* ((tmp (/
                   (+ (* point-x axis-x) (* point-y axis-y))
                   (+ (expt axis-x 2) (expt axis-y 2))))
             (proj-x (* tmp axis-x))
             (proj-y (* tmp axis-y)))
        (+ (* proj-x axis-x) (* proj-y axis-y))))))

(proclaim '(inline project-onto-axis))
(defun project-onto-axis (axis point)
  "Project POINT onto AXIS and compute a scalar value."
  (declare (optimize (speed 3))
           (type point point)
           (type vector2 axis))
  (with-accessors ((point-x point-x) (point-y point-y)) point
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

(let ((axis (make-vector2 :x 0.0 :y 0.0)))
  (defun axis-project-overlap (p1 p2 polygon1 polygon2)
    "T if the points in POLYGON1 and POLYGON2 overlap once projected onto the
   face normal of the face between P1 and P2."
    (declare (optimize (speed 3))
             (point p1 p2)
             ((simple-array point) polygon1 polygon2))
    (let ((poly1-max nil) (poly1-min nil)
          (poly2-max nil) (poly2-min nil))
      (setf (slot-value axis 'x) (- (point-x p1) (point-x p2))
            (slot-value axis 'y) (- (point-y p1) (point-y p2)))
      (loop for point across polygon1 do
           (let ((distance (the world-position (distance-along-axis axis point))))
             (when (or (null poly1-max) (> distance poly1-max))
               (setf poly1-max distance))
             (when (or (null poly1-min) (< distance poly1-min))
               (setf poly1-min distance))))
      (loop for point across polygon2 do
           (let ((distance (the world-position (distance-along-axis axis point))))
             (when (or (null poly2-max) (> distance poly2-max))
               (setf poly2-max distance))
             (when (or (null poly2-min) (< distance poly2-min))
               (setf poly2-min distance))))
      (and (< poly2-min poly1-max) (>= poly2-max poly1-min)))))

;;;; Non-consing matrix utils

(defmacro %set-vector-data (result &rest data)
  (alexandria:once-only (result)
    `(progn
       ,@(loop :for i :from 0 :below (length data) :collect
              `(setf (elt ,result ,i) ,(elt data i)))
       ,result)))

(defmacro %set-matrix-data (result
                            m11 m12 m13 m14
                            m21 m22 m23 m24
                            m31 m32 m33 m34
                            m41 m42 m43 m44)
  `(%set-vector-data ,result
                     ,m11 ,m21 ,m31 ,m41
                     ,m12 ,m22 ,m32 ,m42
                     ,m13 ,m23 ,m33 ,m43
                     ,m14 ,m24 ,m34 ,m44))

(let ((product-matrix (sb-cga:identity-matrix)))
  (defun n-matrix* (result &rest matrices)
    (declare (optimize (speed 3))
             (dynamic-extent matrices)
             ((simple-array single-float (16)) result product-matrix))
    (%set-matrix-data product-matrix
                      1f0 0f0 0f0 0f0
                      0f0 1f0 0f0 0f0
                      0f0 0f0 1f0 0f0
                      0f0 0f0 0f0 1f0)
    (labels ((mref (matrix row column)
               (declare ((simple-array single-float (16)) matrix))
               (aref matrix (+ row (* column 4))))
             ((setf mref) (value matrix row column)
               (declare ((simple-array single-float (16)) matrix))
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
               (loop :for i :from 0 :below (length result) :do
                    (setf (elt result i) 0.0))
               (inline-mul product-matrix matrix result)
               (loop :for i :from 0 :below (length result) :do
                    (setf (elt product-matrix i) (elt result i)))))))
    result))

(defun n-translate* (result x y z)
  "Construct a translation matrix from translation factors X, Y and Z."
  (declare (optimize (speed 3))
           (single-float x y z)
           ((simple-array single-float (16)) result))
  (%set-matrix-data result
                    1f0 0f0 0f0 x
                    0f0 1f0 0f0 y
                    0f0 0f0 1f0 z
                    0f0 0f0 0f0 1f0))

(let ((tmp-matrix (sb-cga:identity-matrix)))
  (declare (optimize (speed 3))
           ((simple-array single-float (16)) tmp-matrix))
  (defun n-rotate* (result x y z)
    (declare (optimize (speed 3))
             ((simple-array single-float (16)) result)
             (single-float x y z))
    (%set-matrix-data result
                      1f0 0f0 0f0 0f0
                      0f0 1f0 0f0 0f0
                      0f0 0f0 1f0 0f0
                      0f0 0f0 0f0 1f0)
    (unless (= 0f0 z)
      (let ((c (cos z))
            (s (sin z)))
        (setf result (n-matrix* result
                              (%set-matrix-data
                               tmp-matrix
                               c     (- s) 0f0    0f0
                               s     c     0f0    0f0
                               0f0   0f0   1f0    0f0
                               0f0   0f0   0f0    1f0)))))
    (unless (= 0f0 y)
      (let ((c (cos y))
            (s (sin y)))
        (setf result (n-matrix* result
                              (%set-matrix-data
                               tmp-matrix
                               c     0f0   s      0f0
                               0f0   1f0   0f0    0f0
                               (- s) 0f0   c      0f0
                               0f0   0f0   0f0    1f0)))))
    (unless (= 0f0 x)
      (let ((c (cos x))
            (s (sin x)))
        (setf result (n-matrix* result
                              (%set-matrix-data
                               tmp-matrix
                               1f0   0f0   0f0    0f0
                               0f0   c     (- s)  0f0
                               0f0   s     c      0f0
                               0f0   0f0   0f0    1f0)))))
    result))

(defun n-scale* (result x y z)
  (declare (optimize (speed 3))
           (single-float x y z)
           ((simple-array single-float (16)) result))
  (%set-matrix-data
   result
   x    0f0  0f0  0f0
   0f0  y    0f0  0f0
   0f0  0f0  z    0f0
   0f0  0f0  0f0  1f0))

(defun n-ortho-matrix (result-matrix left right bottom top near far)
  (declare (optimize (speed 3))
           (world-position left right bottom top near far)
           ((simple-array single-float (16)) result-matrix))
  (let ((r-l (- right left))
        (t-b (- top bottom))
        (f-n (- far near)))
    (%set-matrix-data
     result-matrix
     (/ 2.0 r-l) 0.0 0.0 (- (/ (+ right left) r-l))
     0.0 (/ 2.0 t-b) 0.0 (- (/ (+ top bottom) t-b))
     0.0 0.0 (/ -2.0 f-n) (- (/ (+ far near) f-n))
     0.0 0.0 0.0 1.0)))
