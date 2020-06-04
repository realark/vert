(in-package :recurse.vert)

(defstruct matrix-interpolator
  "Struct used to interpolate matrices with caching."
  ;; matrix from the previous update frame
  (0matrix (identity-matrix) :type matrix)
  ;; slot to store cached interpolated matrix
  (imatrix (identity-matrix) :type matrix)
  ;; the update-percent last used to calculate imatrix
  (cached-update-percent -1.0 :type single-float)
  (matrix-changed-p nil :type boolean))

(defun interpolator-compute (interpolator 1matrix update-percent)
  "Construct an interpolation matrix"
  (declare (optimize (speed 3))
           (matrix 1matrix)
           (matrix-interpolator interpolator)
           ((single-float 0.0 1.0) update-percent))
  (with-accessors ((0matrix matrix-interpolator-0matrix)
                   (imatrix matrix-interpolator-imatrix)
                   (cached matrix-interpolator-cached-update-percent)
                   (changed-p matrix-interpolator-matrix-changed-p))
      interpolator
    (unless (= cached update-percent)
      (setf cached update-percent)
      (cond ((sb-cga:matrix~ 0matrix 1matrix)
             (setf changed-p nil)
             (copy-array-contents 1matrix imatrix))
            (t (let ((interpolated-matrix (interpolate-matrix
                                           0matrix
                                           1matrix
                                           update-percent)))
                 (declare (dynamic-extent interpolated-matrix))
                 (setf changed-p t)
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

@inline
(defun obb-render-transform (obb)
  "Construct a rendering transform for OBB."
  (declare (optimize (speed 3))
           (obb obb))
  (let ((dimensions (scale-matrix (width obb)
                                  (height obb)
                                  1.0)))
    (declare (dynamic-extent dimensions))
    (matrix*
     (local-to-world-matrix obb)
     dimensions)))

(defclass interpolated-obb (obb)
  ((interpolator0-dirty-p :initform t)
   (interpolator1-dirty-p :initform t)
   (interpolator :initform (make-matrix-interpolator)
                 :documentation "Holds the previous transform of the game-object. Used for interpolating between update frames."))
  (:documentation "An OBB which stores its previous transform matrix and may interpolate a transform between the OBB's current position and previous positon.
Useful in rendering because often rendering time wants to show a state in between the current and previous update frames."))

(defmethod initialize-instance :after ((obb interpolated-obb) &rest args)
  (declare (optimize (speed 3))
           (ignore args))
  (with-slots (interpolator) obb
    (let ((m (obb-render-transform obb)))
      (declare (dynamic-extent m))
      (interpolator-update interpolator m))))

(defun interpolated-obb-get-transform (obb &key (update-percent 1.0))
  "Get the transform matrix for the obb. 1.0 = current transform. 0.0 = last frame's transform.
In-between 0 and 1 will interpolate an appropriate transform matrix."

  ;; FIXME
  (obb-render-transform obb)
  )
