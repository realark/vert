(in-package :recurse.vert)

@export-class
(defclass game-object (event-publisher)
  ((object-id :initform (gensym "object-id-")
              :initarg :object-id
              :reader object-id
              :documentation "id unique to each game-object instance in a scene as tested by EQUALP.
User may provide this, but if they do so they are responsible for guaranteeing uniqueness.")
   (object-name :initform nil
                :initarg :object-name
                :reader object-name
                :documentation "Optional, human-readable name for an object."))
  (:documentation "Base class of all game objects."))

(defmethod print-object ((object game-object) out)
  (with-slots (object-name object-id) object
    (print-unreadable-object (object out :type t)
      (format out
              "~A{~A}"
              (if object-name
                  (format nil "~A::" object-name)
                  "")
              object-id))))

@export-class
(defclass static-object ()
  ()
  (:documentation "A marker class stating that the object does not require updates (i.e. doesn't move or change any state).
This is an optimization to allow scenes to skip updates for certain objects."))

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
  (:documentation "GAME-OBJECT's clockwise rotation about its center (radians)."))
(defgeneric (setf rotaton) (value game-object))
(defgeneric width (game-object)
  (:documentation "GAME-OBJECT's width in world units."))
(defgeneric (setf width) (value game-object))
(defgeneric height (game-object)
  (:documentation "GAME-OBJECT's height in world units."))
(defgeneric (setf height) (value game-object))
@export
(defgeneric pre-update (game-object)
  (:documentation "Called on each game object before anything in the update frame happens.")
  (:method ((game-object game-object))))

(declaim (ftype (function (t) world-position) x y z)
         (ftype (function (t) world-dimension) width height)
         (ftype (function (t) world-dimension) scale-x scale-y)
         (ftype (function (game-object) rotation-radians) rotation))

;; object movement event publishing
(defevent object-moved (moved-game-object)
    "Called when an object changes its position or dimensions.")

@export
(defgeneric update (object)
  (:documentation "Update OBJECT")
  (:method (object)))

@export
(defgeneric render (game-object update-percent camera rendering-context)
  (:documentation "Render GAME-OBJECT into RENDERING-CONTEXT relative to CAMERA.
UPDATE-PERCENT is percentage [0,1) between calls to the UPDATE method, with 0 being right at the update frame.")
  (:method ((game-object game-object) update-percent camera rendering-context)))

@export
(defgeneric recycle (game-object)
  (:documentation "Instruct GAME-OBJECT to reset any internal state.
Useful in object caching cases where the object was previously in the world and
needs to be re-used as if it was a fresh object (e.g. cached particle). ")
  (:method ((game-object game-object))
    (declare (ignore game-object))))

@export
(defgeneric color (game-object)
  (:documentation "Color adjustment of a game-object. Nil or *white* == no adjustment.")
  (:method ((game-object game-object))
    (declare (ignore game-object))
    nil))
