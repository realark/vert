;; Audio system api
(in-package :recurse.vert)

(defclass audio-player (event-publisher)
  ()
  (:documentation "Singleton (by convention, not enforced). Stored in *audio* global. Accesses audio resources shared across vert."))

(defgeneric start-audio-player (audio-player)
  (:documentation "Initialize AUDIO-PLAYER."))

(defgeneric stop-audio-player (audio-player)
  (:documentation "Stop AUDIO-PLAYER and release all resources."))

;;;; audio player api

@export
(defgeneric audio-player-copy-state (audio-player &optional destination-audio-state)
  (:documentation "Thread safe. Copy AUDIO-PLAYER's internal audio-state to DESTINATION-AUDIO-STATE."))

@export
(defgeneric audio-player-load-state (audio-player new-audio-state)
  (:documentation "Thread safe. Copy NEW-AUDIO-STATE into  AUDIO-PLAYER's internal audio-state."))

@export
(defgeneric audio-player-load-sfx (audio-player path-to-sfx &key volume)
  (:documentation "Create an object compatible with AUDIO-PLAYER's sound effect playing."))

@export
(defgeneric audio-player-load-music (audio-player path-to-music)
  (:documentation "Create an object compatible with AUDIO-PLAYER's music playing."))

@export
(defgeneric audio-player-play-sound-effect (audio-player path-to-sfx-file &key rate volume)
  (:documentation "Play the audio in PATH-TO-SFX-FILE as a sound effect on the audio player.")
  (:method ((null-audio null) path-to-sfx &key rate volume)
    (declare (ignore null-audio path-to-sfx rate volume))
    'noop))

@export
(defgeneric audio-player-play-music (audio-player path-to-music-file &key num-plays)
  (:documentation "Play the given song. If NUM-REPEATS is -1 the song will loop forever.")
  (:method ((null-audio null) path-to-music-file &key num-plays)
    (declare (ignore null-audio path-to-music-file num-plays))
    'noop))

@export
(defgeneric audio-player-pause (audio-player &key pause-state)
  (:documentation "Pause or unpause all audio.
PAUSE-STATE must be :toggle :pause or :unpause"))

@export
(defgeneric audio-player-stop-music (audio-player)
  (:documentation "Stop any playing music on AUDIO-PLAYER. No effect if no music playing."))

@export
(defgeneric audio-player-stop-sfx (audio-player)
  (:documentation "Stop any playing sound effects on AUDIO-PLAYER."))

@export
(defgeneric audio-player-output-info (audio-player)
  (:documentation "Get the output frequency (hz), bit depth, and number of channels of this audio player.")
  (:method (audio-player)
    ;; just using the hardcoded values
    (values +output-frequency-hz+ +output-bit-depth+ +output-num-channels+)))

;;;; audio system

@export
(defvar *audio*
  nil
  "Active audio player.")

(defconstant +output-frequency-hz+ 44100 "Audio system output frequency.")
(defconstant +output-bit-depth+ 16 "Audio system output bit depth.")
(defconstant +output-num-channels+ 2 "Stereo output")

(defun start-audio-system ()
  (when *audio*
    (start-audio-player *audio*)
    *audio*))

(defun stop-audio-system ()
  (when *audio*
    (stop-audio-player *audio*))
  (values))
