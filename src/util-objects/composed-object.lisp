(in-package :recurse.vert)

(defclass composed-object (game-object)
  ((sub-objects
    :initarg :objects
    :initform (make-array 2 :adjustable T :initial-element nil :fill-pointer 0)))
  (:documentation "A game-object which is composed of smaller sub-object."))

(defmethod initialize-instance :after ((composed composed-object) &rest args)
  (declare (ignore args))
  (loop for sub across (slot-value composed 'sub-objects) do
       (pin-to sub composed)))

(defgeneric add-object (composed-object game-object)
  (:documentation "Add GAME-OBJECT to COMPOSED-OBJECT")
  (:method ((composed-object composed-object) (game-object game-object))
    (vector-push-extend game-object (slot-value composed-object 'sub-objects))
    (pin-to game-object composed-object)))

(defmethod update ((composed-object composed-object) delta-t-ms world-context)
  (loop for sub across (slot-value composed-object 'sub-objects) do
       (update sub delta-t-ms world-context))
  (call-next-method composed-object delta-t-ms world-context))

(defcollision ((composed-object composed-object) (game-object game-object))
  (declare (optimize (space 3)))
  (loop for sub across (the (vector game-object)
                            (slot-value composed-object 'sub-objects))
     do
       (when (and (%aabb-collision-check sub game-object)
                  (collidep sub game-object))
         (return T))))

(defcollision ((polygon convex-polygon) (composed-object composed-object))
  (declare (optimize (space 3)))
  (loop for sub across (the (vector game-object)
                            (slot-value composed-object 'sub-objects))
     do
       (when (and (%aabb-collision-check sub polygon)
                  (collidep sub polygon))
         (return T))))

(defmethod collision :after ((composed-object composed-object) (game-object game-object))
  (loop for sub across (the (vector game-object)
                            (slot-value composed-object 'sub-objects))
     do
       (when (and (%aabb-collision-check sub game-object)
                  (collidep sub game-object))
         (collision game-object sub)
         (return T))))

(defmethod collision :after ((game-object game-object) (composed-object composed-object))
  (loop for sub across (the (vector game-object)
                            (slot-value composed-object 'sub-objects))
     do
       (when (and (%aabb-collision-check sub game-object)
                  (collidep sub game-object))
         (collision game-object sub)
         (return T))))

(defmethod render ((composed-object composed-object) update-percent camera rendering-context)
  (loop for sub across (slot-value composed-object 'sub-objects) do
       (render sub update-percent camera rendering-context)))

(defmethod load-resources ((composed-object composed-object) rendering-context)
  (loop for sub across (slot-value composed-object 'sub-objects) do
       (load-resources sub rendering-context)))

(defmethod release-resources ((composed-object composed-object))
  (loop for sub across (slot-value composed-object 'sub-objects) do
       (release-resources sub)))

(defmethod recycle ((composed-object composed-object))
  (loop for sub across (slot-value composed-object 'sub-objects) do
       (recycle sub)))
