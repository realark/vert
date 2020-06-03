(in-package :recurse.vert)

;;;; application-window interface

(defclass application-window ()
  ()
  (:documentation "Application window drawn by the OS' window manager."))

(defgeneric resize-window (application-window width-pixels height-pixels)
  (:documentation "Resize APPLICATION-WINDOW to WIDTH x HEIGHT.")
  (:method :after ((application-window application-window) width height)
           (after-resize-window application-window width height)))

(defgeneric after-resize-window (application-window width-pixels height-pixels)
  (:method ((application-window application-window) width height)
    ;; inform the camera of the active-scene of the window resize
    (when (and *scene* (camera *scene*))
      (multiple-value-bind (width-pixels height-pixels)
          (window-size-pixels application-window)
        (setf (screen-width (camera *scene*)) width-pixels
              (screen-height (camera *scene*)) height-pixels)))))

(defgeneric window-size-pixels (application-window)
  (:documentation "Returns width and height of the application-window in pixels."))

(defgeneric toggle-fullscreen (application-window)
  (:documentation "Toggle fullscreen on application-window."))

;;;; sdl application window

(defclass sdl-application-window (application-window)
  ((sdl-window :initarg :sdl-window
               :initform (error ":sdl-window required")
               :type sdl2-ffi:sdl-window
               :accessor sdl-window
               :documentation "Underlying sdl window")
   (pre-fs-width :initform 0)
   (pre-fs-height :initform 0)
   (pre-fs-x :initform 0)
   (pre-fs-y :initform 0))
  (:documentation "SDL2 application window"))

(defmethod window-size-pixels ((application-window sdl-application-window))
  (sdl2:get-window-size (sdl-window application-window)))

(defmethod resize-window (application-window width-pixels height-pixels)
  (with-slots ((win sdl-window))
      application-window
    (sdl2:set-window-size win width-pixels height-pixels)))

(defmethod after-resize-window :after ((application-window sdl-application-window) width-pixels height-pixels)
  (set-gl-viewport-to-game-resolution width-pixels height-pixels))

(defun compute-gl-viewport-for-game-resolution (width-pixels height-pixels)
  "compute values that scale the games resolution inside the given width and height.
returns (values x y w h) to be used to set the gl:viewport.
The viewport will be scaled to the max size that fits inside the input width and height.
The viewport will be centered into the middle of the window defined by the input width and height."
  (destructuring-bind (resolution-width resolution-height)
      (or (getconfig 'game-resolution *config*)
          '(320 180))
    (declare (fixnum resolution-width resolution-height width-pixels height-pixels))
    ;; first scale down to min resolution
    (let ((divisor (gcd resolution-width resolution-height)))
      (setf resolution-width (/ resolution-width divisor)
            resolution-height (/ resolution-height divisor)))
    ;; then scale up to as many of the min dimensions can fit into the area
    (let ((scaling-factor (min (floor width-pixels resolution-width)
                               (floor height-pixels resolution-height))))
      (setf resolution-width
            (* resolution-width scaling-factor)
            resolution-height
            (* resolution-height scaling-factor)))
    (values
     (floor (- width-pixels resolution-width) 2)
     (floor (- height-pixels resolution-height) 2)
     resolution-width
     resolution-height)))

(defun set-gl-viewport-to-game-resolution (width-pixels height-pixels)
  (multiple-value-bind (x y w h)
      (compute-gl-viewport-for-game-resolution width-pixels height-pixels)
    ;; TODO: Centering the render area causes a strange bug with gl-pipelines when multiple effects are present.
    ;; (gl:viewport x y w h)
    (gl:viewport 0 0 w h)))
(defmethod toggle-fullscreen ((application-window sdl-application-window))
  (with-slots ((win sdl-window) pre-fs-width pre-fs-height pre-fs-x pre-fs-y) application-window
    (let ((currently-fullscreen? (member :FULLSCREEN (sdl2:get-window-flags win))))
      (multiple-value-bind (_ max-screen-width max-screen-height)
          (sdl2:get-current-display-mode 0)
        (declare (ignore _))
        (if currently-fullscreen?
            (progn
              (sdl2:set-window-fullscreen win nil)
              (resize-window application-window pre-fs-width pre-fs-height)
              (sdl2:set-window-position win pre-fs-x pre-fs-y)
              (sdl2:show-cursor))
            (progn
              (multiple-value-bind (x y) (sdl2:get-window-position win)
                (multiple-value-bind (width-px height-px) (window-size-pixels application-window)
                  (setf pre-fs-width width-px
                        pre-fs-height height-px
                        pre-fs-x x
                        pre-fs-y y)))
              (resize-window application-window max-screen-width max-screen-height)
              (handler-case
                  (sdl2:set-window-fullscreen win T)
                (sdl2::sdl-error (sdl-error)
                  (log:error "unable to set \"real\" fullscreen mode: ~A~%Falling back to :desktop. See https://wiki.libsdl.org/SDL_SetWindowFullscreen for details."
                             sdl-error)
                  (sdl2:set-window-fullscreen win :desktop)))
              ;; put mouse in window center to avoid triggering corner effects
              ;; on certain desktops
              (sdl2:warp-mouse-in-window
               win
               (floor max-screen-width 2)
               (floor max-screen-height 2))
              (sdl2:hide-cursor)))))))
