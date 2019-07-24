(in-package :recurse.vert)

@export-class
(defclass object-manager (game-object)
  ()
  (:documentation "A game object which manages other game objects."))

@export
(defgeneric get-managed-objects (object-manager)
  (:method ((object-manager object-manager)) '()))

(defmethod remove-from-scene :after ((scene game-scene) (object-manager object-manager))
  (loop :for managed-obj :in (get-managed-objects object-manager) :do
       (remove-from-scene scene managed-obj)))

(defmethod add-to-scene :after ((scene game-scene) (object-manager object-manager))
  (loop :for managed-obj :in (get-managed-objects object-manager) :do
       (add-to-scene scene managed-obj)))
