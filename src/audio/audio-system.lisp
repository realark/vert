(in-package :recurse.vert)

;; TODO: allow replacing engine components in a less hacky way
(defparameter *audio-player-init-fn* nil)

(defparameter *audio-player* nil "Active Audio Player.
This may change after a call to START-AUDIO-SYSTEM. Do not hold a reference to this variable. use GET-AUDIO-PLAYER to access.")

;; hardcoding the audio output format
(defparameter +output-frequency-hz+ 44100 "Audio system output frequency.")
(defparameter +output-bit-depth+ 16 "Audio system output bit depth.")
(defparameter +output-num-channels+ 2 "Stereo output")

(defun start-audio-system ()
  (stop-audio-system)
  (setf *audio-player*
        (if *audio-player-init-fn*
            (funcall *audio-player-init-fn*)
            (make-instance 'sdl-audio-player)))
  (start-audio-player *audio-player*)
  *audio-player*)

(defun stop-audio-system ()
  (when *audio-player*
    (stop-audio-player *audio-player*)
    (setf *audio-player* nil))
  (values))
