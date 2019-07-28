(in-package :recurse.vert)

(defvar +game-loop-thread-name+ "vert-game-loop")

(defparameter *engine-manager*
  nil
  "Manages the current scene and runs the game loop")

(defun main (scene-creator-function &key (block T)
                                      (dev-mode nil)
                                      (audio-player (make-instance 'sdl-audio-player))
                                      (config *default-config*))
  "Start the game engine. No effect if the engine is already started.
If BLOCK is non-nil, the calling thread will block until the game finishes."
  (declare (config config)
           ((or null config) dev-mode)
           (audio-player audio-player)
           (function scene-creator-function))
  (unless *engine-manager*
    #+os-macosx
    (unless (and (eq (or #+sbcl (sb-thread:main-thread)
                         (error "unable to find main thread for lisp impl ~A:~A"
                                (lisp-implementation-type)
                                (lisp-implementation-version)))
                     (current-thread))
                 block)
      (error "osx will crash if any thread other than thread0 issues drawing calls"))
    (setf *engine-manager* (make-instance 'sdl-engine-manager)
          *config* config
          *dev-mode* dev-mode
          *audio* audio-player)
    (assert-units)
    (labels ((run-engine ()
               (unwind-protect
                    (sdl2:make-this-thread-main
                     (lambda ()
                       (run-game *engine-manager* scene-creator-function)))
                 (setf *engine-manager* nil
                       *config* nil
                       *dev-mode* nil)
                 (sb-ext:gc :full T))))
      (if block
          (run-engine)
          (make-thread
           #'run-engine
           :name +game-loop-thread-name+)))))

(defun quit ()
  "Stop the running game engine."
  (when *engine-manager*
    (quit-engine *engine-manager*)))
