(in-package :recurse.vert)

@export-class
(defclass game-object (event-publisher)
  ((object-id :initform nil
              :initarg :object-id
              :reader object-id))
  (:documentation "Base class of all game objects."))

;; object's world position, dimension, and rotation info.
;; will be implemented by physics component

(defgeneric x (game-object)
  (:documentation "GAME-OBJECT's upper-left x location in the game world."))
(defgeneric (setf x) (value game-object))
(defgeneric y (game-object)
  (:documentation "GAME-OBJECT's upper-left y location in the game world."))
(defgeneric (setf y) (value game-object))
(defgeneric z (game-object)
  (:documentation "GAME-OBJECT's upper-left z location in the game world."))
(defgeneric (setf z) (value game-object))
(defgeneric rotation (game-object)
  (:documentation "GAME-OBJECT's clockwise rotation in radians."))
(defgeneric (setf rotaton) (value game-object))
(defgeneric width (game-object)
  (:documentation "GAME-OBJECT's width in world units."))
(defgeneric (setf width) (value game-object))
(defgeneric height (game-object)
  (:documentation "GAME-OBJECT's height in world units."))
(defgeneric (setf height) (value game-object))
(defgeneric interpolate-position (game-object update-percent)
  (:documentation "Return an interpolated (x y z) for GAME-OBJECT at UPDATE-PERCENT between its last two update states."))
(defgeneric pre-update (game-object)
  (:documentation "Called on each game object before anything in the update frame happens.")
  (:method ((game-object game-object))))

(declaim (ftype (function (t) world-position) x y z)
         (ftype (function (t) world-dimension) width height)
         (ftype (function (game-object) rotation-radians) rotation))

;; object movement event publishing
(defevent object-moved (moved-game-object)
    "Called when an object changes its position or dimensions.")

;; game update methods
;; subclasses will hook :before, :after, and :around
(defgeneric update-input (game-object delta-t-ms world-context)
  (:documentation "Update the object based on external input.")
  (:method ((game-object game-object) delta-t-ms world-context)
    ;; default no-op
    (declare (ignore game-object delta-t-ms world-context))))

(defgeneric update-user (game-object delta-t-ms world-context)
  (:documentation "User defined update.

To add behavior, it is recommended to add an :after for the specific class which owns the new behavior.

For example, to add new type of ai to an enemy class, add an :after method which dispatches
on the enemy class then implements the enemy ai.

This allows game classes to re-use behavior by extending the classes with the desired
UPDATE-USER functionality.")
  (:method ((game-object game-object) delta-t-ms world-context)
    ;; default no-op
    (declare (ignore game-object delta-t-ms world-context))))

(defgeneric update-motion (game-object delta-t-ms world-context)
  (:documentation "Update the object's position")
  (:method ((game-object game-object) delta-t-ms world-context)
    ;; default no-op
    (declare (ignore game-object delta-t-ms world-context))))

(defgeneric update (game-object delta-t-ms world-context)
  (:documentation "Update GAME-OBJECT for an elapsed time of DELTA-T-MS inside of WORLD-CONTEXT.")
  (:method ((game-object game-object) delta-t-ms world-context)
    (update-input game-object delta-t-ms world-context)
    (update-user game-object delta-t-ms world-context)
    (update-motion game-object delta-t-ms world-context)))

;; rendering methods to be implemented by graphics component

(defgeneric render (game-object update-percent camera rendering-context)
  (:documentation "Render GAME-OBJECT into RENDERING-CONTEXT relative to CAMERA.
UPDATE-PERCENT is percentage [0,1) between calls to the UPDATE method, with 0 being right at the update frame.")
  (:method ((game-object game-object) update-percent camera rendering-context)
    (declare (ignore game-object update-percent camera rendering-context))))

(defgeneric load-resources (game-object rendering-context)
  (:documentation "Called once before any rendering takes place.
Allow GAME-OBJECT load resources with RENDERING-CONTEXT (E.g. load a resource from disk).")
  (:method ((game-object game-object) rendering-context)
    (declare (ignore game-object rendering-context))))

(defgeneric release-resources (game-object)
  (:documentation "GAME-OBJECT will no longer be rendered.
Release any open resources and clean up state.")
  (:method ((game-object game-object))
    (declare (ignore game-object))))

(defgeneric recycle (game-object)
  (:documentation "GAME-OBJECT has left the world but will be reused later.
Useful for components that do their own memory management (e.g. particle manager)")
  (:method ((game-object game-object))
    (declare (ignore game-object))))

(defgeneric color (game-object)
  (:documentation "Color adjustment of a game-object. Nil or *white* == no adjustment.")
  (:method ((game-object game-object))
    (declare (ignore game-object))
    nil))
