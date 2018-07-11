;;;; AI and player objects for game maps.

(in-package :recurse.vert)

(defclass agent (game-object) ()
  (:documentation "Marker class for AI or human controlled game-object."))

(defevent killed ((agent agent))
    "An agent is killed.")
