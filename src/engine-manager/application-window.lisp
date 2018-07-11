(in-package :recurse.vert)

(defclass application-window ()
  ()
  (:documentation "Application window drawn by the OS' window manager."))

(defgeneric resize-window (application-window width-pixels height-pixels)
  (:documentation "Resize APPLICATION-WINDOW to WIDTH x HEIGHT.")
  (:method :after ((application-window application-window) width height)
           (%after-resize-window application-window width height)))

(defgeneric %after-resize-window (application-window width-pixels height-pixels)
  (:method ((application-window application-window) width height)
    ;; inform the camera of the active-scene of the window resize
    (when (and *engine-manager*
               (active-scene *engine-manager*)
               (camera (active-scene *engine-manager*)))
      (let ((camera (camera (active-scene *engine-manager*))))
        (multiple-value-bind (width-pixels height-pixels)
            (window-size-pixels application-window)
          (setf (screen-width camera) width-pixels
                (screen-height camera) height-pixels))))))


(defgeneric window-size-pixels (application-window)
  (:documentation "Returns width and height of the application-window in pixels."))

(defgeneric toggle-fullscreen (application-window)
  (:documentation "Toggle fullscreen on application-window."))
