;; Generic audio player methods
(in-package :recurse.vert)

(defclass audio-player (event-publisher)
  ((music-state :initform :stopped
                :type keyword
                :accessor music-state
                :documentation "State of the music. Must be :playing, :paused, or :stopped.
The setf also allows for a :toggle option.")
   (num-music-plays :initform 0
                    :documentation "number of times to repeat the current music")
   (current-music :initform nil
                  :reader current-music
                  :documentation "path to the current music"))
  (:documentation "Class for playing music or sound effects."))

(defgeneric start-audio-player (audio-player)
  (:documentation "Initialize AUDIO-PLAYER.
Must be called before any sounds will play."))

(defgeneric stop-audio-player (audio-player)
  (:documentation "Stop AUDIO-PLAYER and release all resources."))

;;;; audio player api

(defgeneric play-sound-effect (audio-player path-to-sfx-file &key rate)
  (:documentation "Play the audio in PATH-TO-SFX-FILE as a sound effect on the audio player."))

(defgeneric play-music (audio-player path-to-music-file &key num-plays)
  (:documentation "Play the given song. If NUM-REPEATS is -1 the song will loop forever."))

(defmethod (setf music-state) :around (new-state audio-player)
  (with-slots (current-music) audio-player
    (ecase new-state
      (:playing
       (unless current-music
         (error "Invalid music-state. No song to play")))
      (:paused
       (unless current-music
         (error "Invalid music-state. No song to pause")))
      (:stopped)))
  (call-next-method new-state audio-player))

(defgeneric audio-output-info (audio-player)
  (:documentation "Get the output frequency (hz), bit depth, and number of channels of this audio player.")
  (:method (audio-player)
    ;; just using the hardcoded values
    (values +output-frequency-hz+ +output-bit-depth+ +output-num-channels+)))
