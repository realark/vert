(in-package :recurse.vert)

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
    (sdl2:set-window-size win width-pixels height-pixels)
    (gl:viewport 0 0 width-pixels height-pixels)))

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
              (sdl2:set-window-fullscreen win T)
              (sdl2:hide-cursor)))))))
