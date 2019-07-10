(in-package :recurse.vert)

(defstruct matrix-interpolator
  "Struct used to interpolate matrices with caching."
  ;; matrix from the previous update frame
  (0matrix (identity-matrix) :type matrix)
  ;; slot to store cached interpolated matrix
  (imatrix (identity-matrix) :type matrix)
  ;; the update-percent last used to calculate imatrix
  (cached-update-percent -1.0 :type single-float))

(defun interpolator-compute (interpolator 1matrix update-percent)
  "Construct an interpolation matrix"
  (declare (optimize (speed 3))
           (matrix 1matrix)
           (matrix-interpolator interpolator)
           ((single-float 0.0 1.0) update-percent))
  (with-accessors ((0matrix matrix-interpolator-0matrix)
                   (imatrix matrix-interpolator-imatrix)
                   (cached matrix-interpolator-cached-update-percent))
      interpolator
    (unless (= cached update-percent)
      (setf cached update-percent)
      (cond ((sb-cga:matrix~ 0matrix 1matrix)
             (copy-array-contents 1matrix imatrix))
            (t (let ((interpolated-matrix (interpolate-matrix
                                           0matrix
                                           1matrix
                                           update-percent)))
                 (declare (dynamic-extent interpolated-matrix))
                 (copy-array-contents interpolated-matrix imatrix)))))
    imatrix))

(defun interpolator-update (interpolator new-0matrix)
  (declare (matrix-interpolator interpolator))
  (with-accessors ((0matrix matrix-interpolator-0matrix)
                   (cached matrix-interpolator-cached-update-percent))
      interpolator
    (copy-array-contents new-0matrix 0matrix)
    (setf cached -1.0)
    (values)))
