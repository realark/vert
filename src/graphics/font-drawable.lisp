(in-package :recurse.vert)

@export
(defclass font-drawable (gl-font aabb)
  ;; duplicate slots to use for defaults of cached characters
  ()
  (:documentation "A drawable which loads pixels from a font and user-defined text"))
