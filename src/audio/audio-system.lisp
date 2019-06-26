(in-package :recurse.vert)

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
