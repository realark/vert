(in-package :recurse.vert)

@export-class
(defclass game-object (event-publisher)
  ((pinned-objects :initform nil)
   (pinned-size-objects :initform nil)
   (object-id :initform nil
              :initarg :object-id
              :reader object-id)
   (motion-lock :initform nil
                :documentation "When non-nil, the game-object is in an intermediary position.
OBJECT-MOVED-ALL events will fire, but OBJECT-MOVED will not."))
  (:documentation "Base class of all game objects."))

;; object pinning

(defgeneric pin-to (object object-to-pin-to &optional pin-size)
  (:documentation "Pin OBJECT to OBJECT-TO-PIN-TO.
Afterwards, OBJECT match all of OBJECT-TO-PIN-TO's movements.")
  (:method ((object game-object) (object-to-pin-to game-object) &optional pin-size)
    (with-slots (pinned-objects pinned-size-objects) object-to-pin-to
      (unless (find object pinned-objects)
        (push object pinned-objects))
      (when pin-size
        (unless (find object pinned-size-objects)
          (push object pinned-size-objects)))
      object)))

(defgeneric unpin-from (object object-to-unpin-from)
  (:documentation "Unpin OBJECT from OBJECT-TO-UNPIN-FROM.")
  (:method (object (object-to-unpin-from game-object))
    (with-slots (pinned-objects) object-to-unpin-from
      (setf pinned-objects (delete object pinned-objects))
      object)))

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
  (:documentation "Called on each game object before anything in the update frame happens."))

;; object movement event publishing
(defevent object-moved (moved-game-object)
    "Called when an object changes its x, y, z, or rotation.")

(defevent object-moved-all (moved-game-object)
    "Like OBJECT-MOVED event, but also fires for intermediary stages (e.g. every step in collision resolution.).
Subscribers to this event need not sub OBJECT-MOVED, as OBJECT-MOVED-ALL is a superset.")

(defmethod object-moved :around ((game-object game-object))
  (object-moved-all game-object)
  (unless (slot-value game-object 'motion-lock)
    (call-next-method game-object)))

(defmacro with-motion-lock (game-object &body body)
  (alexandria:once-only (game-object)
    (alexandria:with-gensyms (motion-lock)
      `(with-slots ((,motion-lock motion-lock)) ,game-object
         (unwind-protect
              (progn
                (when ,motion-lock (error "motion already locked"))
                (setf ,motion-lock T)
                ,@body)
           (setf ,motion-lock nil)
           (object-moved ,game-object))))))

;; all these :arounds do the same thing:
;; 1. move pinned objects to match delta movement
;; 2. invoke object-moved method
(defmethod (setf x) :around (value (object game-object))
  (let* ((old-val (x object))
         (result (call-next-method value object))
         (delta (- (x object) old-val)))
    (loop for pinned-object in (slot-value object 'pinned-objects)
       do (incf (x pinned-object) delta))
    (object-moved object)
    result))

(defmethod (setf y) :around (value (object game-object))
  (let* ((old-val (y object))
         (result (call-next-method value object))
         (delta (- (y object) old-val)))
    (loop for pinned-object in (slot-value object 'pinned-objects)
       do (incf (y pinned-object) delta))
    (object-moved object)
    result))

(defmethod (setf z) :around (value (object game-object))
  (let* ((old-val (z object))
         (result (call-next-method value object))
         (delta (- (z object) old-val)))
    (loop for pinned-object in (slot-value object 'pinned-objects)
       do (incf (z pinned-object) delta))
    (object-moved object)
    result))

(defmethod (setf rotation) :around (value (object game-object))
  (let* ((old-val (rotation object))
         (result (call-next-method value object))
         (delta (- (rotation object) old-val)))
    (loop for pinned-object in (slot-value object 'pinned-objects)
       do (incf (rotation pinned-object) delta))
    (object-moved object)
    result))

(defmethod (setf width) :around (value (object game-object))
  (let* ((old-val (width object))
         (result (call-next-method value object))
         (delta (- (width object) old-val)))
    (loop :for pinned-object :in (slot-value object 'pinned-size-objects) :do
         (setf (width pinned-object)
               (max #.(expt 10.0 -3)
                    (+ (width pinned-object) delta))))
    (object-moved object)
    result))

(defmethod (setf height) :around (value (object game-object))
  (let* ((old-val (height object))
         (result (call-next-method value object))
         (delta (- (height object) old-val)))
    (loop :for pinned-object :in (slot-value object 'pinned-size-objects) :do
         (setf (height pinned-object)
               (max #.(expt 10.0 -3)
                    (+ (height pinned-object) delta))))
    (object-moved object)
    result))

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
  (:documentation "")
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

(defgeneric color-mod (game-object)
  (:documentation "Color adjustment of a game-object. May be nil for no adjustment.")
  (:method ((game-object game-object))
    (declare (ignore game-object))
    nil))
