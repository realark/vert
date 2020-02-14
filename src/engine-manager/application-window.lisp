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
  (destructuring-bind (game-resolution-w game-resolution-h)
      (getconfig 'initial-window-size *config*)
    (labels ((find-nearest-matching-resolution ()
               (let* ((resolution (/ game-resolution-w game-resolution-h))
                      (w (numerator resolution))
                      (h (denominator resolution)))
                 (loop :for i :from 2 :do
                      (when (or (> (* i w) width-pixels)
                                (> (* i h) height-pixels))
                        (return (list (* (- i 1) w)
                                      (* (- i 1) h))))
                      (when (> i 1000)
                        (error "Failed to compute game resolution"))))))
      (destructuring-bind (matching-resolution-w matching-resolution-h)
          (find-nearest-matching-resolution)
        (assert (= (/ matching-resolution-w matching-resolution-h)
                   (/ game-resolution-w game-resolution-h)))
          (let* ((delta-w (- width-pixels matching-resolution-w))
                 (delta-h (- height-pixels matching-resolution-h)))
            #+nil
            (format t "win:~Ax~A~%nearest:~Ax~A~%delta:~Ax~A~%~%"
                    width-pixels height-pixels
                    matching-resolution-w matching-resolution-h
                    delta-w delta-h)
            (gl:viewport (/ delta-w 2)
                         (/ delta-h 2)
                         (- width-pixels delta-w)
                         (- height-pixels delta-h)))))))

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
              ;; put mouse in window center to avoid triggering corner effects
              ;; on certain desktops
              (sdl2:warp-mouse-in-window
               win
               (floor max-screen-width 2)
               (floor max-screen-height 2))
              (sdl2:hide-cursor)))))))
