(in-package :recurse.vert)

@export-slots
(defstruct (animation (:constructor %make-animation))
  "Stateless animation struct"
  (spritesheet (error ":spritesheet required") :type string)
  (time-between-frames-ms 50 :type (integer 0 *))
  (frames (error ":frames required") :type (simple-vector *)))

@export
(defmacro make-animation (&rest args)
  ;; TODO Validate :frames
  `(%make-animation ,@args))

@export-class
(defclass animated-sprite (static-sprite)
  ((path-to-sprite :initform nil)
   (animations :initarg :animations
               :reader animations
               :initform (error ":animations required")
               :documentation "plist. key = :animation-name, val = animation-struct")
   (active-animation :initform nil)
   (active-animation-keyword :initform nil)
   (next-frame-change-timestamp :initform nil
                                :documentation "ms timestamp when current frame will be swapped for a new one.")
   (active-animation-frame-index :initform 0
                                 :accessor active-animation-frame-index
                                 :documentation "pointer to the currently rendered animation frame."))
  (:documentation "A sprite which contains at least one animation."))

(defmethod initialize-instance :after ((animated-sprite animated-sprite) &rest args)
  (declare (ignore args))
  (with-slots (animations
               active-animation
               next-frame-change-timestamp)
      animated-sprite
    (setf next-frame-change-timestamp 0
          active-animation (getf animations (get-new-animation animated-sprite))
          (active-animation-frame-index animated-sprite) 0
          (path-to-sprite animated-sprite) (animation-spritesheet active-animation))))

(defmethod (setf active-animation-frame-index) :after (new-index (animated-sprite animated-sprite))
  (declare (optimize (speed 3)))
  (with-slots (active-animation
               next-frame-change-timestamp
               active-animation-frame-index)
      animated-sprite
    (incf (the fixnum next-frame-change-timestamp)
          (the fixnum (animation-time-between-frames-ms active-animation)))
    (let ((frame (elt (animation-frames active-animation) active-animation-frame-index)))
      (setf (sprite-source animated-sprite) frame))))

(defmethod update ((animated-sprite animated-sprite))
  (declare (optimize (speed 3)))
  (prog1 (call-next-method animated-sprite)
    (flet ((has-next-frame (animation current-frame-index)
             (declare (animation animation) ((integer 0 1000) current-frame-index))
             (< current-frame-index (- (length (animation-frames animation)) 1))))
      (with-slots (animations
                   active-animation
                   active-animation-keyword
                   next-frame-change-timestamp)
          animated-sprite
        (let ((now (scene-ticks *scene*))
              (new-animation-keyword (get-new-animation animated-sprite)))
          (declare (timestamp-ms now next-frame-change-timestamp))
          (when (or (>= now next-frame-change-timestamp)
                    (not (eq active-animation-keyword new-animation-keyword)))
            (if (and (has-next-frame active-animation (active-animation-frame-index animated-sprite))
                     (eq active-animation-keyword new-animation-keyword))
                (incf (the (integer 0 10000)
                           (active-animation-frame-index animated-sprite)))
                (setf active-animation-keyword new-animation-keyword
                      active-animation (getf animations active-animation-keyword)
                      (path-to-sprite animated-sprite) (animation-spritesheet active-animation)
                      next-frame-change-timestamp now ; frame index setter below will update the next update time correctly relative to now
                      (active-animation-frame-index animated-sprite) 0))))))))

@export
(defgeneric interrupt-animation (game-object)
  (:documentation "Cancel the current animation and get a new one")
  (:method ((animated-sprite animated-sprite))
    (with-slots (animations
                 active-animation-frame-index
                 active-animation
                 next-frame-change-timestamp
                 active-animation-keyword)
        animated-sprite
      (setf active-animation-keyword (get-new-animation animated-sprite)
            active-animation (getf animations active-animation-keyword)
            next-frame-change-timestamp 0
            active-animation-frame-index 0)
      active-animation-keyword)))

@export
(defgeneric get-new-animation (game-object)
  (:documentation "Called every draw updated.
A keyword present in the animation plist must be returned.
If this keyword is the same keyword previously returned the animation will continue.
Otherwise the animation will be interrupted and the new animation will begin.")
  (:method ((animated-sprite animated-sprite))
    (first (animations animated-sprite))))
