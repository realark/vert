(in-package :recurse.vert)

;; TODO: Remove Dead Collisions System Code after testing is completed
;; collision system

(defclass collision-system ()
  ((next-check-min-x :initform nil)
   (next-check-max-x :initform nil)
   (next-check-min-y :initform nil)
   (next-check-max-y :initform nil)
   (next-check-min-z :initform nil)
   (next-check-max-z :initform nil)
   (next-check-objects :initform (make-array 0
                                             :fill-pointer 0
                                             :adjustable t
                                             :element-type 'game-object)
                       :documentation "Objects which have moved in the current frame and will be checked for collisions at the end of the frame.")
   (current-check-objects :initform (make-array 0
                                                :fill-pointer 0
                                                :adjustable t
                                                :element-type 'game-object)
                          :documentation "Vector used during collision checking."))
  (:documentation "Global Collision System."))

(defun collision-system-object-moved (collision-system object)
  (declare (optimize (speed 3))
           (collision-system collision-system)
           (obb object))
  (with-slots (next-check-objects) collision-system
    (declare ((vector game-object) next-check-objects))
    (unless (find object next-check-objects)
      (vector-push-extend object next-check-objects)))
  (block set-min-maxes
    (multiple-value-bind (x y z w h)
        (world-dimensions object)
      (with-slots (next-check-min-x
                   next-check-max-x
                   next-check-min-y
                   next-check-max-y
                   next-check-min-z
                   next-check-max-z)
          collision-system
        (declare (single-float x y z w h)
                 ((or null single-float)
                  next-check-min-x
                  next-check-max-x
                  next-check-min-y
                  next-check-max-y
                  next-check-min-z
                  next-check-max-z))
        (when (or (null next-check-min-x)
                  (< x next-check-min-x))
          (setf next-check-min-x x))
        (when (or (null next-check-max-x)
                  (> (+ x w) next-check-max-x))
          (setf next-check-max-x (+ x w)))

        (when (or (null next-check-min-y)
                  (< y next-check-min-y))
          (setf next-check-min-y y))
        (when (or (null next-check-max-y)
                  (> (+ y h) next-check-max-y))
          (setf next-check-max-y (+ y h)))

        (when (or (null next-check-min-z)
                  (< z next-check-min-z))
          (setf next-check-min-z z))
        (when (or (null next-check-max-z)
                  (> z next-check-max-z))
          (setf next-check-max-z z)))))
  object)

(defvar *collision-system* (make-instance 'collision-system))

(defmethod update ((collision-system collision-system))
  (declare (optimize (speed 3)))
  (with-slots (next-check-objects
               current-check-objects
               next-check-min-x
               next-check-max-x
               next-check-min-y
               next-check-max-y
               next-check-min-z
               next-check-max-z)
      collision-system
    (declare ((or null single-float)
              next-check-min-x
              next-check-max-x
              next-check-min-y
              next-check-max-y
              next-check-min-z
              next-check-max-z)
             ((vector game-object) next-check-objects current-check-objects))
    ;; Assumes:
    ;; - no collisions in the previous frame.
    ;; - Running collision resolution on an object will eventually reset to its previous location

    ;; performance is critical here, so let's pay attention to iteration
    ;; A = all objects in spatial partition
    ;; N = all moved objects (to run collision checks on)

    (loop :for i :from 0
       ;; keep repeating until no objects have been moved by collision checking/resolution
       :while (> (length next-check-objects) 0) :do
         (when (> (the fixnum i) 99)
           (log:error "Could not resolve collisions after ~A attempts. Giving up and continuing. This is likely a bug.~%Unresolved objects: ~A"
                      i
                      next-check-objects)
           (setf (fill-pointer next-check-objects) 0
                 next-check-min-x nil
                 next-check-max-x nil
                 next-check-min-y nil
                 next-check-max-y nil
                 next-check-min-z nil
                 next-check-max-z nil)
           (return))
         (log:trace "collision check iteration: ~A" i)
         (let ((min-x next-check-min-x)
               (max-x next-check-max-x)
               (min-y next-check-min-y)
               (max-y next-check-max-y)
               (min-z next-check-min-z)
               (max-z next-check-max-z))
           (block reset-for-next-phase
             (rotatef current-check-objects next-check-objects)
             (setf (fill-pointer next-check-objects) 0
                   next-check-min-x nil
                   next-check-max-x nil
                   next-check-min-y nil
                   next-check-max-y nil
                   next-check-min-z nil
                   next-check-max-z nil))
           (do-spatial-partition (object
                                  (spatial-partition *scene*)
                                  :min-x min-x
                                  :max-x max-x
                                  :min-y min-y
                                  :max-y max-y
                                  :min-z min-z
                                  :max-z max-z)
             ;; Iteration of A
             ;; = A
             (loop :for moved-object :across current-check-objects :do
                ;; Iteration of N
                  (when (and (not (eq moved-object object))
                             (collidep moved-object object))
                    ;; moved-object-has-collision
                    (test-collision-resolution moved-object object)
                    ;; resolution potentially moved moved-object again , so re-check in the next collision check iteration
                    (collision-system-object-moved
                     collision-system
                     moved-object)
                    ;; other moved objects may now be in collision with the resolved object. Re-check them in the next iteration if needed
                    (loop :for other-moved-object :across current-check-objects :do
                         (when (and (not (eq moved-object other-moved-object))
                                    (collidep moved-object other-moved-object))
                           (collision-system-object-moved
                            collision-system
                            other-moved-object))))))))))

;; Testing collision system with kinematic-objects
(defun test-collision-resolution (moving-object stationary-object)
  (with-slots (original-position) moving-object
    (collision moving-object stationary-object)
    (linear-resolution moving-object
                       stationary-object
                       :original-position original-position)))

;; collision defs and checks

(defvar *collision-defs* (make-hash-table) "hash of collisions defined with defcollision.")

(defun assert-collisions-defined (class &rest classes-to-check)
  (loop with defined-collisions = (gethash class *collision-defs*)
     for other-class in classes-to-check do
       (unless (find other-class defined-collisions)
         (error "missing defcollision for ~A ~A"
                class
                other-class))
     finally (return class)))

(defgeneric collidep (object1 object2)
  (:documentation "Returns T if OBJECT1 is in collision with OBJECT2.
The order of the arguments MUST NOT matter.
i.e. (collidep object1 object2) <==> (collidep object2 object1)."))

(defmacro defcollision (((object1 object1-class) (object2 object2-class)) &body body)
  "Define the collision condition between two objects. Returns non-nil if OBJECT1 is in collision with OBJECT2.

It is only required to use the macro once per unique type pairing.
For example, if you define a collision for (TYPE1 TYPE2), do not define one for (TYPE2 TYPE1)."
  (loop for name-or-class in (list object1 object1-class object2 object2-class) do
       (unless (symbolp name-or-class)
         (error "Expected symbol. Got ~A" name-or-class)))
  `(progn
     (let ((type1-defs (gethash ',object1-class *collision-defs*))
           (type2-defs (gethash ',object2-class *collision-defs*)))
       (unless (find ',object2-class type1-defs)
         (push ',object2-class type1-defs)
         (setf (gethash ',object1-class *collision-defs*) type1-defs))
       (unless (or (eq ',object1-class ',object2-class)
                   (find ',object1-class type2-defs))
         (push ',object1-class type2-defs)
         (setf (gethash ',object2-class *collision-defs*) type2-defs)))
     (defmethod collidep ((,object1 ,object1-class) (,object2 ,object2-class))
       ,@body)
     ;; reverse so the method body is invoked regardless of arg order
     ,(unless (eq object1-class object2-class)
        `(defmethod collidep ((,object2 ,object2-class) (,object1 ,object1-class))
           (collidep ,object1 ,object2)))))

(defgeneric collision (moving-object stationary-object)
  (:documentation "Called when two objects collide.
Invoked once per collision; after collision resolution.")
  (:method (moving-object stationary-object)))

;; collision resolution
(defmacro defcollision-resolution (name ((moving-object-name moving-object-type)
                                         (stationary-object-name stationary-object-type)
                                         &rest args)
                                   &body body)
  "Define how to resolve a collision between a moving and stationary object."
  (loop for name-or-class in (append args (list name moving-object-name moving-object-type
                                                stationary-object-name stationary-object-type))
     do (unless (symbolp name-or-class)
          (error "Expected symbol. Got ~A" name-or-class)))
  `(defmethod ,name ((,moving-object-name ,moving-object-type)
                     (,stationary-object-name ,stationary-object-type)
                     ,@args)
     ,@body))

@export
(defmacro with-collision-check ((moving-object physics-context)
                                (position-update-keyword &body position-update-body)
                                (on-collision-keyword stationary-object
                                                      &body collision-resolution-body))
  "Specify the block of code which will update the MOVING-OBJECT's position
inside of PHYSICS-CONTEXT and call COLLISION-RESOLUTION-NAME if any collisions occur."
  (assert (eq position-update-keyword :position-update))
  (assert (eq on-collision-keyword :on-collision))
  (assert (symbolp stationary-object))
  (alexandria:once-only (moving-object)
    (alexandria:with-gensyms (game-obj x y z w h)
      `(progn
         ,@position-update-body
         ;; object's position has updated. Now check for collisions
         (multiple-value-bind (,x ,y ,z ,w ,h)
             (world-dimensions ,moving-object)
           (do-spatial-partition (,game-obj
                                  (spatial-partition ,physics-context)
                                  :min-x (- ,x *collision-precision*)
                                  :max-x (+ ,x ,w *collision-precision*)
                                  :min-y (- ,y *collision-precision*)
                                  :max-y (+ ,y ,h *collision-precision*)
                                  :min-z ,z
                                  :max-z ,z)
             (unless (eq ,game-obj ,moving-object)
               (when (collidep ,moving-object ,game-obj)
                 (let ((,stationary-object ,game-obj))
                   (collision ,moving-object ,stationary-object)
                   ,@collision-resolution-body)))))
         (values)))))

(defgeneric favored-collision-resolution-axis (moving-object stationary-object)
  (:documentation "If both x and y axis collide, use this method to favor one over the other. X to preserver the x-motion, Y to Preserve the y, or nil for no preference.")
  (:method (moving-object stationary-object) nil))

@export
(defgeneric hit-box (game-object other-object)
  (:documentation "Returns a game-object which will be used to check for collisions between GAME-OBJECT and OTHER-OBJECT. This object must be entirely inside GAME-OBJECT
The default method should be good enough for most game objects. Extend this method to provide specific hit-box logic between two objects (for example, to provide a smaller hit-box for player<>enemy collision checks).")
  (:method ((object game-object) other-object) object))

;; if hit-boxes are defined on either object, use them to determine if there was a collision.
(defmethod collidep :around ((obj1 game-object) (obj2 game-object))
  (declare (optimize (speed 3)))
  (labels ((any-element-collides-p (object-array other-object)
             (declare ((simple-array game-object) object-array)
                      (game-object other-object))
             ;; t if any elements of the array collide
             (loop :for obj :across (the (simple-array game-object) object-array) :do
                  (when (collidep obj other-object)
                    (return t))))
           (any-hit-boxes-collidep (boxes1 boxes2)
             (cond ((and (typep boxes1 '(simple-array game-object))
                         (typep boxes2 '(simple-array game-object)))
                    (loop :for obj1 :across (the (simple-array game-object) boxes1) :do
                         (when (loop :for obj2 :across (the (simple-array game-object) boxes2) :do
                                    (when (collidep obj1 obj2)
                                      (return t)))
                           (return t))))
                   ((typep boxes1 'simple-array)
                    (any-element-collides-p boxes1 boxes2))
                   ((typep boxes2 'simple-array)
                    (any-element-collides-p boxes2 boxes1))
                   (t (collidep boxes1 boxes2)))))
    (let ((hit-box1 (hit-box obj1 obj2))
          (hit-box2 (hit-box obj2 obj1)))
      (and (call-next-method obj1 obj2)
           (or (and (eq obj1 hit-box1)
                    (eq obj2 hit-box2))
               (any-hit-boxes-collidep hit-box1 hit-box2))))))

;;;; phantom

@export-class
(defclass phantom ()
  ()
  (:documentation "An object which can collide with other objects, but other objects cannot collide with it.
COLLISION callbacks will still be invoked when objects collide with a phantom, but no collision resolution will occur."))

(defmethod collidep :around ((phantom1 phantom) (phantom2 phantom))
  nil)

(defmethod collidep :around (other-object (phantom phantom))
  (when (call-next-method other-object phantom)
    (collision other-object phantom))
  nil)

;;;; halfa

@export-class
(defclass halfa ()
  ((phantom-p :initarg phantom-p
              :initform nil
              :accessor phantom-p))
  (:documentation "An object which may toggle its phantom collision property."))

(defmethod collidep :around ((halfa1 halfa) (halfa2 halfa))
  (if (and (phantom-p halfa1) (phantom-p halfa2))
      nil
      (call-next-method halfa1 halfa2)))

(defmethod collidep :around (other-object (halfa halfa))
  (if (phantom-p halfa)
      (progn
        (when (call-next-method other-object halfa)
          (collision other-object halfa))
        nil)
      (call-next-method other-object halfa)))
