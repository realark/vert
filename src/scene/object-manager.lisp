(in-package :recurse.vert)

@export-class
(defclass object-manager (game-object)
  ()
  (:documentation "A game object which manages other game objects."))

(defmethod get-managed-objects ((object-manager object-manager))
  nil)
