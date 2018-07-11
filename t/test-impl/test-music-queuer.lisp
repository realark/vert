(in-package :recurse.vert/test)
;; test object which queues music samples in the audio thread

(defclass test-music-queuer (test-object)
  ((amplitude-queue :initform
                    (sb-concurrency:make-queue :name 'amplitudes)
                    :documentation "time ordered queue of sound wave amplitudes.
Front of queue = oldest. Queue item = (cons left right)"))
  (:documentation "test object which queues music samples in the audio thread"))


(defevent-callback music-advance ((audio-player audio-player)
                                  (listener test-music-queuer)
                                  music-position-ms)
  (declare (ignore audio-player listener music-position-ms)))

(defevent-callback sample-process ((audio-player audio-player)
                                   (listener test-music-queuer)
                                   left-amp
                                   right-amp)
  (declare (ignore right-amp))
  (sb-concurrency:enqueue left-amp (slot-value listener 'amplitude-queue)))
