(in-package :recurse.vert)

(defclass transform (game-object)
  ((parent :initform nil
           :documentation "The parent transform of this matrix, or nil for world coordinates.")
   (transform-children
    :initform (make-array 0
                          :element-type 'transform
                          :adjustable t
                          :fill-pointer 0)
    :accessor transform-children
    :documentation "objects positioned relative to this transform.")
   (local-position :initform (vector3 0.0 0.0 0.0)
                   :documentation "position in local space")
   (local-rotation :initform 0.0
                   :documentation "rotation in local space")
   (local-scale :initform (vector2 1.0 1.0))
   (dirty-p :initform t)
   (local-to-world-matrix :initform (identity-matrix))
   (inverse-dirty-p :initform t)
   (world-to-local-matrix :initform (identity-matrix)))
  (:documentation "A 2d game object which may be positioned relative to other game-objects."))

(defmethod initialize-instance :after ((transform transform)
                                       &key
                                         (parent nil)
                                         (x 0.0)
                                         (y 0.0)
                                         (z 0.0)
                                         (scale-x 1.0)
                                         (scale-y 1.0)
                                         (rotation 0.0))
  (setf (x transform) (coerce x 'single-float)
        (y transform) (coerce y 'single-float)
        (z transform) (coerce z 'single-float)
        (scale-x transform) (coerce scale-x 'single-float)
        (scale-y transform) (coerce scale-y 'single-float)
        (rotation transform) (coerce rotation 'single-float))
  (when parent
    (setf (parent transform) parent)))

(defun %mark-dirty (transform)
  (declare (transform transform))
  (with-slots (dirty-p inverse-dirty-p transform-children) transform
    (fire-event transform object-moved)
    (setf dirty-p t
          inverse-dirty-p t)
    (loop :for child :across transform-children :do
         (%mark-dirty child))))

@export
(defmethod parent ((transform transform))
  (slot-value transform 'parent))

(defmethod (setf parent) (new-parent (transform transform))
  (declare ((or null transform) new-parent))
  (flet ((add-child (parent child)
           (with-slots (transform-children) parent
             (if (find child transform-children)
                 (error "~A already a child of ~A" child parent)
                 (vector-push-extend child transform-children))))
         (remove-child (parent child)
           (with-slots (transform-children) parent
             (if (find child transform-children)
                 (setf transform-children (delete child transform-children))
                 (error "~A not a child of ~A" child parent)))))
    (with-slots (parent) transform
      (unless (eq new-parent parent)
        (loop :with p = new-parent
           :while (not (null p)) :do
             (when (eq p transform)
               (error "~A cannot parent itself" transform))
             (setf p (parent p)))

        (when parent
          (remove-child parent transform))
        (setf parent new-parent)
        (when parent
          (add-child parent transform))
        (%mark-dirty transform)))))

@inline
(defun %local-to-parent-matrix (transform)
  (declare (optimize (speed 3))
           (transform transform))
  (with-slots (local-position local-rotation local-scale) transform
    (let* ((translation (translation-matrix
                         (x local-position)
                         (y local-position)
                         (z local-position)))
           (to-r-center (translation-matrix (* 0.5 (scale-x transform) (width transform))
                                            (* 0.5 (scale-y transform) (height transform))
                                            0.0))
           (away-r-center (translation-matrix (* -0.5 (scale-x transform) (width transform))
                                              (* -0.5 (scale-y transform) (height transform))
                                              0.0))
           (rotation (rotation-matrix 0f0 0f0 local-rotation))
           (scale (scale-matrix (x local-scale)
                                (y local-scale)
                                1.0))
           (product (matrix*
                     translation
                     ;; use translation matrices to rotate around the object's local center
                     to-r-center
                     rotation
                     away-r-center
                     scale)))
      (declare (dynamic-extent translation rotation scale to-r-center away-r-center))
      product)))

(defmethod local-to-world-matrix ((transform transform))
  (declare (optimize (speed 3)))
  (with-slots (dirty-p parent local-to-world-matrix) transform
    (when dirty-p
      (let ((local-to-parent (%local-to-parent-matrix transform )))
        (declare (dynamic-extent local-to-parent))
        (if (null parent)
            (copy-array-contents local-to-parent local-to-world-matrix)
            (let ((product (matrix* (local-to-world-matrix parent) local-to-parent)))
              (declare (dynamic-extent product))
              (copy-array-contents product local-to-world-matrix))))
      (setf dirty-p nil))
    local-to-world-matrix))

(defmethod world-to-local-matrix ((transform transform))
  (declare (optimize (speed 3)))
  (with-slots (inverse-dirty-p world-to-local-matrix) transform
    (when inverse-dirty-p
      (let ((inverse (inverse-matrix (local-to-world-matrix transform))))
        (declare (dynamic-extent inverse))
        (copy-array-contents inverse world-to-local-matrix))
      (setf inverse-dirty-p nil))
    world-to-local-matrix))

@export
@inline
(defun transform-point (vector from-basis &optional to-basis)
  "Construct a position vector created from translating VECTOR oriented in FROM-BASIS to TO-BASIS' local coordinates (or null for world coords)"
  (declare (optimize (speed 3))
           ((or vector2 vector3) vector)
           (transform from-basis)
           ((or null transform) to-basis))
  (let* ((z (if (typep vector 'vector2) 0f0 (z vector)))
         (vec-matrix
          (translation-matrix (x vector)
                              (y vector)
                              z))
         (tmp (identity-matrix))
         (product (matrix* (if to-basis
                               (world-to-local-matrix to-basis)
                               tmp)
                           (local-to-world-matrix from-basis)
                           vec-matrix)))
    (declare (dynamic-extent vec-matrix product tmp))
    (vector3 (sb-cga:mref product 0 3)
             (sb-cga:mref product 1 3)
             (sb-cga:mref product 2 3))))

;; setters for position, rotation, and dimension

(declaim (ftype (function (transform) vector3) local-position))
(defmethod local-position ((transform transform))
  (slot-value transform 'local-position))

(defmethod x ((transform transform))
  (x (slot-value transform 'local-position)))
(defmethod (setf x) (value (transform transform))
  (unless (eq value (x (slot-value transform 'local-position)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (x (slot-value transform 'local-position))))
      (unless (float= float-value current-value)
        (setf (x (slot-value transform 'local-position)) float-value)
        (%mark-dirty transform)))))

(defmethod y ((transform transform))
  (y (slot-value transform 'local-position)))
(defmethod (setf y) (value (transform transform))
  (unless (eq value (y (slot-value transform 'local-position)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (y (slot-value transform 'local-position))))
      (unless (float= float-value current-value)
        (setf (y (slot-value transform 'local-position)) float-value)
        (%mark-dirty transform)))))

(defmethod z ((transform transform))
  (z (slot-value transform 'local-position)))
(defmethod (setf z) (value (transform transform))
  (unless (eq value (z (slot-value transform 'local-position)))
    (let ((float-value  (coerce value 'single-float))
          (current-value (z (slot-value transform 'local-position))))
      (unless (float= float-value current-value)
        (setf (z (slot-value transform 'local-position)) float-value)
        (%mark-dirty transform)))))

(defmethod rotation ((transform transform))
  (slot-value transform 'local-rotation))
(defmethod (setf rotation) (value (transform transform))
  (setf (slot-value transform 'local-rotation) (coerce value 'single-float))
  (%mark-dirty transform))

(declaim (ftype (function (transform) single-float) scale-x scale-y))

(defmethod scale-x ((transform transform))
  (width (slot-value transform 'local-scale)))
(defmethod (setf scale-x) (value (transform transform))
  (setf (width (slot-value transform 'local-scale)) (coerce value 'single-float))
  (%mark-dirty transform))

(defmethod scale-y ((transform transform))
  (height (slot-value transform 'local-scale)))
(defmethod (setf scale-y) (value (transform transform))
  (setf (height (slot-value transform 'local-scale)) (coerce value 'single-float))
  (%mark-dirty transform))
