(in-package :recurse.vert)

(defclass sdl-engine-manager (engine-manager)
  ((keyboard-input
    :initform (make-instance 'input-device
                             :input-name :keyboard
                             :input-type :keyboard))
   ;; TODO: clean up controller storage
   (sdl-to-vert-controllers
    :documentation "Map sdl-id -> vert-id"
    :initform (make-hash-table :test #'equalp))
   (sdl-controllers :initform (make-hash-table :test #'equalp)))
  (:documentation "Engine manager implemented with sdl2"))

(defmethod (setf clear-color) :after (value (engine-manager sdl-engine-manager))
  (with-accessors ((r r) (g g) (b b) (a a)) (clear-color engine-manager)
    (gl:clear-color r g b a)))

(defmethod render-game-window ((engine-manager sdl-engine-manager))
  (sdl2:gl-swap-window (sdl-window (application-window engine-manager)))
  ;; (gl:clear :depth-buffer-bit :color-buffer-bit)
  (gl:clear :color-buffer-bit)
  (values))

(defmethod run-game ((engine-manager sdl-engine-manager) initial-scene-creator)
  (if (getconfig 'use-dummy-audio-output *config*)
      (sb-posix:setenv "SDL_AUDIODRIVER" "dummy" 1)
      (sb-posix:unsetenv "SDL_AUDIODRIVER"))
  (sdl2:with-init (:everything)
    (progn
      ;; https://wiki.libsdl.org/SDL_GLattr
      ;; https://wiki.libsdl.org/SDL_GLprofile
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask
                        sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:gl-set-attr :doublebuffer 1)
      #+darwin
      (sdl2:gl-set-attr :context-forward-compatible-flag
                        sdl2-ffi:+sdl-gl-context-forward-compatible-flag+))

    (log:info "Running on lisp ~A version ~A~%"
            (lisp-implementation-type)
            (lisp-implementation-version))
    (log:info "Compiled against SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (log:info "opengl version ~A.~A~%profile mask: ~A~%"
            (sdl2:gl-get-attr :CONTEXT-MAJOR-VERSION)
            (sdl2:gl-get-attr :CONTEXT-MINOR-VERSION)
            (sdl2:gl-get-attr :context-profile-mask))

    ;; disabling the compositor makes the framerate look a lot smoother
    ;; but I could eventually add an option to keep the compositor on
    (when (getconfig 'enable-compositor *config*)
      (cffi:with-foreign-strings ((hint-name "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR")
                                  (hint-val "0"))
        ;; prevent SDL from disabling the linux compositor
        (sdl2-ffi.functions:sdl-set-hint hint-name hint-val)))

    (let ((window-flags (remove nil
                                (list :opengl
                                      (when (getconfig 'resizable-window  *config*)
                                        :resizable)
                                      (if (getconfig 'hidden-window *config*) :hidden :shown)))))
      (destructuring-bind (win-w-px win-h-px)
          (getconfig 'initial-window-size *config*)
        (sdl2:with-window (win :w win-w-px :h win-h-px
                               :flags window-flags
                               :title (getconfig 'game-name *config*))
          (when (getconfig 'window-icon *config*)
            (let* ((img-path (resource-path (getconfig 'window-icon *config*)))
                   (soil-image-pointer nil)
                   (surf nil))
              (when img-path
                (unwind-protect
                     (progn
                       (setf surf (multiple-value-bind
                                        (img-pointer width height component-count-file component-count-data)
                                      (cl-soil:load-image img-path :rgba)
                                    (assert (= 4 component-count-file component-count-data))
                                    (setf soil-image-pointer img-pointer)
                                    (sdl2:create-rgb-surface-with-format-from
                                     img-pointer
                                     width
                                     height
                                     (* component-count-data 8)
                                     (* component-count-data width)
                                     :format sdl2:+pixelformat-rgba32+)))
                       (sdl2-ffi.functions:sdl-set-window-icon win surf))
                  (when surf (sdl2:free-surface surf))
                  (when soil-image-pointer (cl-soil:free-image-data soil-image-pointer))))))
          (sdl2:with-gl-context (sdl-glcontext win)
            (sdl2:gl-make-current win sdl-glcontext)
            (gl:viewport 0 0 win-w-px win-h-px)

            (progn                   ; set global gl options
              (when (getconfig 'enable-vsync *config*)
                (when (= -1 (sdl2::sdl-gl-set-swap-interval -1))
                  (sdl2::sdl-gl-set-swap-interval 1)))
              (log:info "window swap interval (vsync): ~A~%" (sdl2:gl-get-swap-interval))
              (gl:enable :cull-face)
              (gl:enable :blend)
              (gl:blend-func :src-alpha :one-minus-src-alpha))

            (setf (slot-value engine-manager 'application-window)
                  (make-instance 'sdl-application-window :sdl-window win)
                  (slot-value engine-manager 'rendering-context)
                  (make-gl-context :wrapper sdl-glcontext)
                  *gl-context* (slot-value engine-manager 'rendering-context))
            (when (getconfig 'fullscreen-p *config*)
              (toggle-fullscreen (slot-value engine-manager 'application-window)))

            ;; show a black window while everything else loads
            (gl:clear-color 0.0 0.0 0.0 1.0)
            (gl:clear :color-buffer-bit)
            (sdl2:gl-swap-window win)

            (register-input-device (input-manager engine-manager)
                                   (slot-value engine-manager 'keyboard-input))

            (flet ((load-sdl-controller-db (path-to-db)
                     (let ((num-mappings-loaded (sdl2:game-controller-add-mappings-from-file path-to-db)))
                       (if (< num-mappings-loaded 0)
                           (progn
                             (log:error "Error loading controller mappings from ~A. sdl error: ~A"
                                        path-to-db
                                        (sdl2-ffi.functions:sdl-get-error))
                             (sdl2-ffi.functions:sdl-clear-error))
                           (log:info "Loaded ~A controller mappings: ~A"
                                     num-mappings-loaded
                                     path-to-db)))))
              (if (getconfig 'controller-db *config*)
                  (load-sdl-controller-db (resource-path (getconfig 'controller-db *config*)))
                  (log:info "No sdl controller db configured")))

            (loop for i from 0 below (sdl2:joystick-count) do
                 (initialize-sdl-controller engine-manager i))

            (call-next-method)))))))

(defmethod cleanup-engine :before ((engine-manager sdl-engine-manager))
  (with-slots (sdl-controllers) engine-manager
    (loop :for sdl-joystick-id :being :the hash-keys :in sdl-controllers :do
         (remove-sdl-controller engine-manager sdl-joystick-id))))

(defmethod quit-engine ((engine-manager sdl-engine-manager))
  (sdl2:push-event :quit))

(on-engine-stop ('clear-gl-context)
  (setf *gl-context* nil))

(defun sdl-key-down (keyboard keysym)
  (log:trace "SDL keyboard down: ~A" (sdl2:scancode keysym))
  (activate-input keyboard (sdl2:scancode keysym)))

(defun sdl-key-up (keyboard keysym)
  (log:trace "SDL keyboard up: ~A" (sdl2:scancode keysym))
  (deactivate-input keyboard (sdl2:scancode keysym)))

(let ((button-id-keywords (make-instance 'cache :test #'equal)))
  ;; write-to-string conses, so we'll save the button-id keywords in a cache

  (defun sdl-controller-button-down (sdl-joystick-id button-id)
    (log:trace "SDL button down: " sdl-joystick-id " -> " button-id)
    (activate-input (gethash sdl-joystick-id (slot-value *engine-manager*
                                                       'sdl-to-vert-controllers))
                    (getcache-default button-id
                                      button-id-keywords
                                      (alexandria:make-keyword (write-to-string button-id)))))

  (defun sdl-controller-button-up (sdl-joystick-id button-id)
    (log:trace "SDL button up: " sdl-joystick-id " -> " button-id)
    (deactivate-input (gethash sdl-joystick-id (slot-value *engine-manager*
                                                         'sdl-to-vert-controllers))
                      (getcache-default button-id
                                        button-id-keywords
                                        (alexandria:make-keyword (write-to-string button-id))))))

(defparameter %mock-button-state%
  (make-instance 'cache :test #'equalp)
  "State to mock button inputs for analog joysticks. controller-id -> axis-id -> button-keyword")

(defun sdl-joystick-movement (controller-id axis-id value)
  ;; TODO: properly handle analog input in the input system instead of mocking dpad
  (log:trace "sdl joystick input: ~A:~A -> ~A" controller-id axis-id value)
  (flet ((set-mock-button (controller-id axis-id button-keyword)
           (setf (getcache axis-id
                           (getcache-default controller-id
                                             %mock-button-state%
                                             (make-instance 'cache :test #'equalp)))
                 button-keyword))
         (get-mock-button (controller-id axis-id)
           (getcache axis-id
                     (getcache-default controller-id
                                       %mock-button-state%
                                       (make-instance 'cache :test #'equalp)))))
    (let ((dead-zone 8000)
          (current-button (get-mock-button controller-id axis-id)))
      (cond ((= 0 axis-id)
             (cond ((and (> value dead-zone) (not (eq :14 current-button)))
                    (log:trace "-- mock right button press")
                    (when current-button
                      (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                                           'sdl-to-vert-controllers))
                                        current-button))
                    (set-mock-button controller-id axis-id :14)
                    (activate-input (gethash controller-id (slot-value *engine-manager*
                                                                       'sdl-to-vert-controllers))
                                    :14))
                   ((and (<= value dead-zone) (eq :14 (get-mock-button controller-id axis-id)))
                    (log:trace "-- mock deactivate right")
                    (set-mock-button controller-id axis-id nil)
                    (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                                         'sdl-to-vert-controllers))
                                      :14))
                   ((and (< value (- dead-zone)) (not (eq :13 current-button)))
                    (log:trace "-- mock left button press")
                    (when current-button
                      (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                                           'sdl-to-vert-controllers))
                                        current-button))
                    (set-mock-button controller-id axis-id :13)
                    (activate-input (gethash controller-id (slot-value *engine-manager*
                                                                       'sdl-to-vert-controllers))
                                    :13))
                   ((and (>= value (- dead-zone)) (eq :13 (get-mock-button controller-id axis-id)))
                    (log:trace "-- mock deactivate left")
                    (set-mock-button controller-id axis-id nil)
                    (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                                         'sdl-to-vert-controllers))
                                      :13))))
            ((= 1 axis-id)
             (cond ((and (> value dead-zone) (not (eq :12 current-button)))
                    (log:trace "-- mock down button press")
                    (set-mock-button controller-id axis-id :12)
                    (activate-input (gethash controller-id (slot-value *engine-manager*
                                                                       'sdl-to-vert-controllers))
                                    :12))
                   ((and (<= value dead-zone) (eq :12 current-button))
                    (log:trace "-- mock deactivate down")
                    (set-mock-button controller-id axis-id nil)
                    (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                                         'sdl-to-vert-controllers))
                                      :12))
                   ((< value (- dead-zone))
                    ;; up
                    (unless (eq :11 (get-mock-button controller-id axis-id))
                      (log:trace "-- mock up button press")
                      (set-mock-button controller-id axis-id :11)
                      (activate-input (gethash controller-id (slot-value *engine-manager*
                                                                         'sdl-to-vert-controllers))
                                      :11)))
                   ((and (>= value (- dead-zone)) (eq :11 current-button))
                    (log:trace "-- mock deactivate up")
                    (set-mock-button controller-id axis-id nil)
                    (deactivate-input (gethash controller-id (slot-value *engine-manager*
                                                                         'sdl-to-vert-controllers))
                                      :11))))))))

(defun initialize-sdl-controller (engine-manager device-index)
  ;; TODO: Send event to input users
  (with-slots (sdl-controllers sdl-to-vert-controllers) engine-manager
    (if (sdl2:game-controller-p device-index)
        (let* ((controller (sdl2:game-controller-open device-index))
               (joy (sdl2:game-controller-get-joystick controller))
               (vert-input-device (make-instance 'input-device
                                                 :input-type :controller
                                                 :input-name
                                                 (or (sdl2:game-controller-name-for-index device-index)
                                                     "generic-controller"))))
          (setf (gethash (sdl2:joystick-instance-id joy) sdl-controllers) controller)
          (setf (gethash (sdl2:joystick-instance-id joy) sdl-to-vert-controllers)
                (register-input-device
                 (input-manager engine-manager)
                 vert-input-device))
          (when (active-scene engine-manager)
            (add-scene-input (active-scene engine-manager) vert-input-device))
          (log:info "SDL Controller added: ~A~%  --device-index=~A~%  --joystick-id=~A"
                    (input-name vert-input-device)
                    device-index
                    (sdl2:joystick-instance-id joy)))
        (log:warn "Unknown input. device-index=~A. No controller input created."
                  device-index))))

(defun remove-sdl-controller (engine-manager sdl-joystick-id)
  ;; TODO: Send event to input users
  (with-slots (sdl-controllers sdl-to-vert-controllers) engine-manager
    (sdl2:game-controller-close (gethash sdl-joystick-id sdl-controllers))
    (remhash sdl-joystick-id sdl-controllers)
    (let ((vert-input-device (gethash sdl-joystick-id sdl-to-vert-controllers)))
      (remhash sdl-joystick-id sdl-to-vert-controllers)
      (log:info "SDL Controller removed: ~A~%  --joystick-id=~A"
                (input-name vert-input-device)
                sdl-joystick-id)
      (when (active-scene engine-manager)
        (remove-scene-input (active-scene engine-manager) vert-input-device)))))

(defmethod run-game-loop ((engine-manager sdl-engine-manager))
  (loop :with quit-p = nil :while (not quit-p) :do
       (restart-case
           (sdl2:with-event-loop (:method :poll)
             (:keydown (:keysym keysym)
                       (sdl-key-down (slot-value engine-manager 'keyboard-input) keysym))
             (:keyup (:keysym keysym)
                     (sdl-key-up (slot-value engine-manager 'keyboard-input) keysym))
             (:controllerdeviceadded (:which device-index)
                                     (initialize-sdl-controller engine-manager device-index))
             (:controllerdeviceremapped (:which id)
                                        ;; The mappings string for the controller id have been updated.
                                        ;; I don't think this will happen in production, just a dev-time things.
                                        ;; For example, happens when a controller mapping is created before a mappings db is read.
                                        (log:warn "Unhandled controller remap: ~A" id))
             (:controllerdeviceremoved (:which sdl-joystick-id)
                                       (remove-sdl-controller engine-manager sdl-joystick-id))
             (:controlleraxismotion (:which controller-id :axis axis-id :value value)
                                    (sdl-joystick-movement controller-id axis-id value))
             (:controllerbuttondown (:which controller-id :button button-id)
                                    (sdl-controller-button-down controller-id button-id))
             (:controllerbuttonup (:which controller-id :button button-id)
                                  (sdl-controller-button-up controller-id button-id))
             (:windowevent (:event event :data1 width :data2 height)
                           (if (= event sdl2-ffi:+sdl-windowevent-size-changed+)
                               (after-resize-window (application-window engine-manager) width height)))
             (:idle () (game-loop-iteration engine-manager))
             (:quit () (setf quit-p t)))
         (continue () :report "Continue Vert Game Loop")
         (reload () :report "Reload game's initial scene"
                 (setf (slot-value engine-manager 'pending-scene-changes) nil)
                 (change-scene engine-manager (funcall (initial-scene-creator-fn engine-manager))))
         (quit () :report "Quit Vert"
               (sdl2:push-event :quit)
               (setf quit-p t)))))
