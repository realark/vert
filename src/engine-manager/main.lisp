(in-package :recurse.vert)

;; TODO: windows sbcl seems to always bind *package* to cl-user for child threads.
;; Using this as a workaround
(defparameter *game-default-package* nil)

(defparameter *engine-manager*
  nil
  "Manages the current scene and runs the game loop")

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
    (unless (and (eq (or #+sbcl (sb-thread:main-thread)
                         (error "unable to find main thread for lisp impl"))
                     (current-thread))
                 block)
      (error "osx will crash if any thread other than thread0 issues drawing calls"))
    (setf *engine-manager* (make-instance 'sdl-engine-manager :game-name game-name))
    (setf *audio-player-init-fn* (lambda () audio-player))
    (setf *game-default-package* *package*)
    (assert-units)
    (labels ((fn ()
               (unwind-protect
                    (sdl2:make-this-thread-main
                     (lambda ()
                       (let ((*package* *game-default-package*))
                         (run-game *engine-manager* scene-creator-function))))
                 (setf *engine-manager* nil)
                 (sb-ext:gc :full T))))
      (if block
          (fn)
          (make-thread
           #'fn
           :name "vert-game-loop")))))

(defun quit ()
  "Stop the running game engine."
  (when *engine-manager*
    (quit-engine *engine-manager*)))
