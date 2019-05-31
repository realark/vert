(in-package :recurse.vert/test)

(defclass test-object (test-introspector obb kinematic-object static-sprite input-handler)
  ((path-to-sprite
    :initform (test-resource-path "rectangle.png")
    :reader path-to-sprite
    :allocation :class)
   (color :initarg :color
          :accessor color
          :initform nil)
   (on-update :initarg :on-update
              :initform nil
              :documentation "function to run every update frame")
   (audio-callback-data :initform (cons nil nil)
                        :accessor audio-callback-data
                        :documentation "(last-song-pos . sample-sum)")
   (num-collisions :initform 0
                   :accessor num-collisions))
  (:documentation "OBB Rectangle used for testing"))

(defmethod initialize-instance :after ((test-object test-object) &rest args)
  (declare (ignore args))
  (setf (color test-object) (color test-object)))

(defmethod (setf color) :after (value (test-object test-object))
  (setf (color test-object) value))

(defmethod collision :after ((test-object test-object) stationary-object)
  (incf (num-collisions test-object)))

(defmethod update :before ((test-object test-object) delta-t-ms world-context)
  (when (slot-value test-object 'on-update)
    (funcall (slot-value test-object 'on-update) test-object))
  (notice-method-invoked test-object "update"))

(defmethod render :before ((test-object test-object) update-percent camera rendering-context)
  (notice-method-invoked test-object "render"))

(defmethod load-resources :before ((test-object test-object) rendering-context)
  (notice-method-invoked test-object "load-resources"))

(defmethod release-resources :before ((test-object test-object))
  (notice-method-invoked test-object "release-resources"))

(defmethod recycle :before ((test-object test-object))
  (notice-method-invoked test-object "recycle"))

(set-default-input-command-map
 test-object
 ("test-keyboard" (:scancode-left :move-left)
                  (:scancode-right :move-right)
                  (:scancode-space :jump)))

(set-default-command-action-map
 test-object
 (:move-left (while-active
              (notice-method-invoked test-object "while-active-move-left")))
 (:move-right (while-active
               (notice-method-invoked test-object "while-active-move-right")))
 (:jump (while-active
         (notice-method-invoked test-object "while-active-jump"))
        (on-deactivate
         (notice-method-invoked test-object "on-deactivate-jump"))))

(defevent-callback music-advance ((audio-player audio-player)
                                  (listener test-object)
                                  music-position-ms)
  (with-slots (audio-callback-data) listener
    (sb-ext:atomic-update (car audio-callback-data)
                          (lambda (old)
                            (declare (ignore old))
                            music-position-ms))))

(defevent-callback sample-process ((audio-player audio-player)
                                   (listener test-object)
                                   left-amp
                                   right-amp)
  (with-slots (audio-callback-data) listener
    (sb-ext:atomic-update (cdr audio-callback-data)
                          (lambda (old)
                            (+ (if old old 0)
                               (- left-amp right-amp))))))
