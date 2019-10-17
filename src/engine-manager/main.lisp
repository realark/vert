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
    (setf *config* config
          *engine-manager* (make-instance 'sdl-engine-manager)
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


(defun %reload-all-shaders ()
  ;; first clear out any existing gl errors
  (gl:get-error)
  (do-cache (*shader-cache* shader-key shader)
    (format T "reload: ~A -- ~A~%" shader-key shader)
    (loop :with shader-loaded = nil
       :while (not shader-loaded) :do
         (restart-case
             ;; wrap in loop so we can retry shader load if there is an error
             (progn (release-resources shader)
                    (load-resources shader t)
                    (setf shader-loaded t))
           (retry-shader-load () :report "Retry loading shader"))))
  (gl-context-clear-all *gl-context*))

;; TODO: generalize resource reloading
@export
(defun reload-all-shaders ()
  (on-game-thread
    (%reload-all-shaders)))
