(in-package :recurse.vert)

(defparameter *engine-manager*
  nil
  "Manages the current scene and runs the game loop")

;; silence incudine logger
(setf (incudine.util:logger-level) :error)

(defun main (scene-creator-function &key (block T)
                                      (dev-mode nil)
                                      (audio-player (make-instance 'sdl-audio-player))
                                      (game-name "Vert-Game"))
  "Start the game engine. No effect if the engine is already started.
If BLOCK is non-nil, the calling thread will block until the game finishes."
  (declare (string game-name))
  (setf *dev-mode* dev-mode)
  (unless *engine-manager*
    #+os-macosx
    (unless (and (eq (find-main-thread) (current-thread)) block)
      (error "osx will crash if any thread other than thread0 issues drawing calls"))

    (setf *engine-manager* (make-instance 'sdl-engine-manager :game-name game-name))
    (setf *audio-player-init-fn* (lambda () audio-player))
    (assert-units)
    (labels ((fn ()
               (unwind-protect
                    (sdl2:make-this-thread-main
                     (lambda ()
                       (run-game *engine-manager* scene-creator-function)))
                 (setf *engine-manager* nil)
                 (sb-ext:gc :full T))))
      (if block
          (unwind-protect
               (fn)
            (setf *engine-manager* nil)
            (sb-ext:gc :full T))
          (make-thread
           #'fn
           :name "vert-thread")))))

(defun quit ()
  "Stop the running game engine."
  (when *engine-manager*
    (quit-engine *engine-manager*)))
