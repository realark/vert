(in-package :recurse.vert)

@export-class
(defclass config ()
  ((%config-hash :initform (make-hash-table)))
  (:documentation "Immutable. Conceptually a map of keys (symbol) to values (T)"))

@export
(defun getconfig (key config)
  (declare (symbol key)
           ((or null config) config))
  (when config
    (gethash key (slot-value config '%config-hash))))

@export
(defmacro make-config ((&optional base-config) &rest key-value-pairs)
  "Create a config out of KEY-VALUE-PAIRS. If supplied all values from BASE-CONFIG will be copied over.

Each pair is a two value list with the first value being a symbol. Any keys present in BASE-CONFIG will be overridden by these values.

Example:
  (make-config (*default-config*) ('foo \"foo val\") ('bar \"bar val\"))"
  (alexandria:with-gensyms (config hash base-hash)
    (alexandria:once-only (base-config)
      `(let ((,config (make-instance 'config)))
         (with-slots ((,hash %config-hash)) ,config
           (when ,base-config
             (with-slots ((,base-hash %config-hash)) ,base-config
               (loop :for config-key :being :the hash-keys :of ,base-hash
                  :using (hash-value config-val)
                  :do (setf (gethash config-key ,hash)
                            (gethash config-key ,base-hash)))))
           ,@(loop :with config-setters = (list)
                :for key-val :in key-value-pairs :do
                  (unless (and (listp key-val) (= 2 (length key-val)))
                    (error "key-value-pairs must be two arg list of (key val). Got ~A" key-val))
                  (push `(setf (gethash (runtime-type-assert
                                         ,(first key-val)
                                         'symbol
                                         "config keys must be symbols")
                                        ,hash)
                               ,(second key-val))
                        config-setters)
                :finally (return (nreverse config-setters)))
           ,config)))))

@export
(defun export-config-key (symbol &optional documentation)
  (export (list symbol))
  (when documentation
    (setf (documentation symbol 'variable) documentation)))

(export-config-key
 'config-resource-dirs "Dirs to check when looking for a resource. See RESOURCE-PATH.")
(export-config-key
 'game-name "Name of the game. Will affect the executable name and window title.")
(export-config-key
 'window-icon "Path to a PNG to use for the window icon. Path must be a relative path to the active config's RESOURCE-PATH. May be nil.")
(export-config-key
 'default-font "Path to a font to use for the default font. Path must be a relative path to the active config's RESOURCE-PATH.")
(export-config-key
 'enable-compositor "Allow running the game window with desktop compositor (e.g. x11 effects). This may affect the game's framerate.")
(export-config-key
 'enable-vsync "Enable vsync for render buffer swapping.")
(export-config-key
 'game-resolution "Resolution to run the game at. (list width-px height-px). Must be integers.")
(export-config-key
 'fullscreen-p "When T, start the game in fullscreen mode")
(export-config-key
 'initial-window-size "A list of of the window's initial width,height. Must be integers.")
(export-config-key
 'initial-window-fullscreen-p "When t, run the game in fullscreen mode")
(export-config-key
 'hidden-window "When t, run the game without an application window. Used to run automated integration tests from the command line.")
(export-config-key
 'resizable-window "Allow application window to be resized. Defaults to t.")
(export-config-key
 'log-output "The keyword :stdout, or a string specifying log's file name.")
(export-config-key
 'log-level "A log4CL log level.")
(export-config-key
 'controller-db "Path to an sdl controller database. See https://wiki.libsdl.org/SDL_GameControllerAddMappingsFromFile")
(export-config-key
 'audio-player-creator-fn "A zero arg function which returns an AUDIO-PLAYER. Use this option to override the built-in sdl-audio-player.")
(export-config-key
 'audio-player-music-volume "Set the music volume. single-float between 0.0 and 1.0. Defaults to 1.0.")
(export-config-key
 'audio-player-sfx-volume "Set the sound effect volume. single-float between 0.0 and 1.0. Defaults to 1.0.")
(export-config-key
 'use-dummy-audio-output "When t, audio will be output to a dummy driver.")

@export
(defvar *default-config* (make-config ()
                                      ('config-resource-dirs (list "./resources"))
                                      ('game-name "VertGame")
                                      ('window-icon nil)
                                      ('default-font "fonts/liberation_sans/LiberationSans-Regular.ttf")
                                      ('enable-vsync t)
                                      ('game-resolution '(320 180))
                                      ('fullscreen-p nil)
                                      ('initial-window-size '(1280 720))
                                      ('resizable-window t)
                                      ('initial-window-fullscreen-p nil)
                                      ('log-output :stdout)
                                      ('log-level :info)
                                      ('controller-db nil)
                                      ('audio-player-creator-fn
                                       (lambda ()
                                         (make-instance 'recurse.vert::sdl-audio-player)))
                                      ('audio-player-music-volume 1.0)
                                      ('audio-player-sfx-volume 1.0))
  "The config used by vert if no config is specified.")

@export
(defvar *config*
  nil
  "The active config")

(defvar *dev-mode*
  nil
  "Set to T to enable dev features (potentially at a cost to performance).")
(defvar *old-dev-mode* nil)

(export-config-key
 'dev-mode-performance-hud "Render a hud with performance stats.")
(export-config-key
 'dev-mode-render-collision-hitboxes "Render collision hitboxes in transparent red and phantoms in transparent blue.")
(export-config-key
 'dev-powersave-render-cap "Powersave feature useful for live dev. Sleep for 100 ms after each game loop iteration.")
(export-config-key
 'dev-live-code-on-game-thread-p "When T and when swank is present, process swank events on the game thread.")

@export
(defun toggle-dev-mode (&optional new-dev-config)
  "Toggle dev mode on the active config"
  (declare ((or null config) new-dev-config))
  (cond (new-dev-config
         (setf *old-dev-mode* nil
               *dev-mode* new-dev-config))
        (*dev-mode* (setf *old-dev-mode* *dev-mode*
                          *dev-mode* nil))
        (t (setf *dev-mode* (or *old-dev-mode*
                                (make-config ()
                                             ('dev-mode-performance-hud t)
                                             ('dev-mode-render-collision-hitboxes nil)))))))

@export
(defun get-dev-config (config-name)
  "Return t if a specific dev-mode option is enabled"
  (and *dev-mode*
       (getconfig config-name *dev-mode*)))
