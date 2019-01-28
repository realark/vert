(in-package :recurse.vert)

@export
(defclass tiled-scene (game-scene)
  ((tiled-map :initarg :tiled-map
              :initform (error ":tiled-map required")
              :documentation "Path to tiled map file."))
  (:documentation "A scene which reads from tiled json files."))

@export
(defgeneric on-tile-read (tiled-scene tile-source-file tile-x tile-y tile-width tile-height)
  (:documentation "Invoked when a tiled tile is read. Implementers will add the appropriate game-object to TILED-SCENE."))
