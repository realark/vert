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
  ((animations :initarg :animations
               :reader animations
               :initform (list)
               :documentation "plist. key = :animation-name, val = animation-struct")
   (active-animation :initform nil)
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
          (active-animation-frame-index animated-sprite) 0)))

(defmethod (setf active-animation-frame-index) :after (new-index (animated-sprite animated-sprite))
  (with-slots (active-animation
               next-frame-change-timestamp
               active-animation-frame-index)
      animated-sprite
    (incf next-frame-change-timestamp (animation-time-between-frames-ms active-animation))
    (let ((source-rect (sprite-source animated-sprite))
          (frame (elt (animation-frames active-animation) active-animation-frame-index)))
      (unless source-rect
        (setf (sprite-source animated-sprite) (make-sprite-source 0 0 1 1)
              source-rect (sprite-source animated-sprite)))
      (setf (sprite-source-x source-rect) (sprite-source-x frame)
            (sprite-source-y source-rect) (sprite-source-y frame)
            (sprite-source-w source-rect) (sprite-source-w frame)
            (sprite-source-h source-rect) (sprite-source-h frame)))))

(defmethod update :after ((animated-sprite animated-sprite) delta-t-ms scene)
  (declare (optimize (speed 3) (space 3)))
  (flet ((has-next-frame (animation current-frame-index)
           (declare (animation animation) ((integer 0 1000) current-frame-index))
           (< current-frame-index (- (length (animation-frames animation)) 1))))
    (with-slots (animations
                 active-animation
                 next-frame-change-timestamp)
        animated-sprite
      (let ((now (scene-ticks scene)))
        (declare (timestamp-ms now next-frame-change-timestamp))
        (when (>= now next-frame-change-timestamp)
          (let ((new-animation (getf animations (get-new-animation animated-sprite))))
            (if (and (has-next-frame active-animation (active-animation-frame-index animated-sprite))
                     (eq active-animation new-animation))
                (incf (the (integer 0 10000)
                           (active-animation-frame-index animated-sprite)))
                (setf active-animation new-animation
                      (path-to-image animated-sprite) (animation-spritesheet active-animation)
                      (active-animation-frame-index animated-sprite) 0))))))))

@export
(defgeneric get-new-animation (game-object)
  (:documentation "Called at the finish of every animation.
  This will be invoked after all user-updates have run.
  Default implementation will just run the first animation it can find.
  return the keyword of the animation to play next.")
  (:method ((animated-sprite animated-sprite))
    (first (animations animated-sprite))))
