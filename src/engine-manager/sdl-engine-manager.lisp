(in-package :recurse.vert)

(defclass sdl-engine-manager (engine-manager)
  ((keyboard-input
    :initform (make-instance 'input-device :input-name "sdl-keyboard"))
   ;; TODO: clean up controller storage
   (sdl-to-vert-controllers
    :documentation "Map sdl-id -> vert-id"
    :initform (make-hash-table :test #'equalp))
   (sdl-controllers :initform (make-hash-table :test #'equalp)))
  (:documentation "Engine manager implemented with sdl2"))

(defmethod (setf clear-color) :after (value (engine-manager sdl-engine-manager))
  (declare (optimize (speed 3)))
  (with-accessors ((r r) (g g) (b b) (a a)) (clear-color engine-manager)
    (sdl2:set-render-draw-color (rendering-context engine-manager)
                                r g b a)))

(defmethod render-game-window ((engine-manager sdl-engine-manager))
  (sdl2:render-present (slot-value engine-manager 'rendering-context))
  (sdl2:render-clear (slot-value engine-manager 'rendering-context)))

(defmethod run-game ((engine-manager sdl-engine-manager) initial-scene-creator)
  (sdl2:with-init (:everything)
    ;; FIXME: Assert version
    (format T "Running on lisp ~A version ~A~%"
            (lisp-implementation-type)
            (lisp-implementation-version))
    (format T "Compiled against SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (format T "opengl version ~A.~A~%profile mask: ~A~%"
            (sdl2:gl-get-attr :CONTEXT-MAJOR-VERSION)
            (sdl2:gl-get-attr :CONTEXT-MINOR-VERSION)
            (sdl2:gl-get-attr :context-profile-mask))

    (sdl2:with-window (win :w 1280 :h 720
                           :flags '(:shown :opengl :resizable)
                           :title (game-name engine-manager))
      (sdl2:with-renderer (renderer win :index -1 :flags '(:accelerated :presentvsync))
        (setf (slot-value engine-manager 'application-window)
              (make-instance 'sdl-application-window :sdl-window win)
              (slot-value engine-manager 'rendering-context)
              renderer)
        (sdl2-image:init '(:png))
        (sdl2-ttf:init)
        (register-input-device (input-manager engine-manager)
                               (slot-value engine-manager 'keyboard-input))
        (loop for i from 0 below (sdl2:joystick-count) do
             (initialize-sdl-controller engine-manager i))
        (call-next-method)))))

(defmethod cleanup-engine :before ((engine-manager sdl-engine-manager))
  (loop :for sdl-joystick-id :being :the hash-keys :in (slot-value engine-manager 'sdl-controllers) :do
       (remove-sdl-controller engine-manager sdl-joystick-id))
  (sdl2-image:quit)
  (sdl2-ttf:quit))

(defmethod quit-engine ((engine-manager sdl-engine-manager))
  (sdl2:push-event :quit))

(defun sdl-key-down (keyboard keysym)
  (activate-input keyboard (sdl2:scancode keysym)))

(defun sdl-key-up (keyboard keysym)
  (deactivate-input keyboard (sdl2:scancode keysym)))

(defun sdl-controller-button-down (controller-id button-id)
  (activate-input (gethash controller-id (slot-value *engine-manager*
                                                     'sdl-to-vert-controllers))
                  (alexandria:make-keyword (write-to-string button-id))))

(defun sdl-controller-button-up (controller-id button-id)
  (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                       'sdl-to-vert-controllers))
                    (alexandria:make-keyword (write-to-string button-id))))

(defun sdl-joystick-movement (controller-id axis-id value)
  (declare (ignore controller-id axis-id value))
  ;; TODO: convert to up/down/left/right keys
  )

(defun initialize-sdl-controller (engine-manager device-index)
  ;; FIXME: Send event to input users
  (with-slots (sdl-controllers sdl-to-vert-controllers) engine-manager
    (when (sdl2:game-controller-p device-index)
      (let* ((controller (sdl2:game-controller-open device-index))
             (joy (sdl2:game-controller-get-joystick controller))
             (vert-input-device (make-instance 'input-device :input-name "controller")))
        (setf (gethash (sdl2:joystick-instance-id joy) sdl-controllers) controller)
        (setf (gethash (sdl2:joystick-instance-id joy) sdl-to-vert-controllers)
              (register-input-device
               (input-manager engine-manager)
               vert-input-device))
        (when (active-scene engine-manager)
          (add-scene-input (active-scene engine-manager) vert-input-device))))))

(defun remove-sdl-controller (engine-manager sdl-joystick-id)
  ;; FIXME: Send event to input users
  (with-slots (sdl-controllers sdl-to-vert-controllers) engine-manager
    (sdl2:game-controller-close (gethash sdl-joystick-id sdl-controllers))
    (remhash sdl-joystick-id sdl-controllers)
    (let ((vert-input-device (gethash sdl-joystick-id sdl-to-vert-controllers)))
      (remhash sdl-joystick-id sdl-to-vert-controllers)
      (when (active-scene engine-manager)
        (remove-scene-input (active-scene engine-manager) vert-input-device)))))

(defmethod run-game-loop ((engine-manager sdl-engine-manager))
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
              (sdl-key-down (slot-value engine-manager 'keyboard-input) keysym))
    (:keyup (:keysym keysym)
            (sdl-key-up (slot-value engine-manager 'keyboard-input) keysym))
    (:controllerdeviceadded (:which device-index)
                            (initialize-sdl-controller engine-manager device-index))
    (:controllerdeviceremapped (:which id)
                               ;; TODO: what does this mean and how do I handle it?
                               (format T "Controller remapped: ~A~%" id))
    (:controllerdeviceremoved (:which sdl-joystick-id)
                              (remove-sdl-controller engine-manager sdl-joystick-id))
    ;; TODO: convert to up/down/left/right keys
    ;;  (:controlleraxismotion (:which controller-id :axis axis-id :value value)
    ;;   (sdl-joystick-movement controller-id axis-id value))
    (:controllerbuttondown (:which controller-id :button button-id)
                           (sdl-controller-button-down controller-id button-id))
    (:controllerbuttonup (:which controller-id :button button-id)
                         (sdl-controller-button-up controller-id button-id))
    (:windowevent (:event event :data1 width :data2 height)
                  (if (= event sdl2-ffi:+sdl-windowevent-size-changed+)
                      (%after-resize-window (application-window engine-manager) width height)))
    (:idle ()
           (restart-case
               (game-loop-iteration engine-manager)
             (continue () :report "Continue Game Loop")))
    (:quit ()
           t)))
