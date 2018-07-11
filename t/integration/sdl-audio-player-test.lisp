(in-package :recurse.vert/integration-test)
;; old sdl-mixer test. No longer active.

(deftest mixer-audio-callback
  ;; only specific sbcl types can be atomic compared-and-swapped
  ;; which is why we're using a cons to pass callback info
  (main (lambda ()
          (let ((listener (make-instance 'test-object))
                (audio-player (audio-player *engine-manager*)))
            (with-accessors ((audio-data audio-callback-data)) listener
              (add-subscriber audio-player listener music-advance sample-process)
              (play-sound-effect audio-player (test-resource-path "silence.wav"))
              (sleep 1)
              (is (car audio-data) nil "SFX does not invoke callback")

              (play-music audio-player (test-resource-path "silence.wav") :num-plays 1)
              (loop while (or (null (car audio-data))
                              (< (car audio-data) 998))
                 for timeout from 0 to 10 do
                   (sleep 1))
              (is (not (null (car audio-data))) T "Music invokes callback.")
              (is (car audio-data) 998 "Callback timed music correctly")
              (is (cdr audio-data) 0 "Callback sees amplitude" :test #'float=)


              (sb-ext:atomic-update (car audio-data)
                                    (lambda (old)
                                      (declare (ignore old))
                                      0))
              (sb-ext:atomic-update (cdr audio-data)
                                    (lambda (old)
                                      (declare (ignore old))
                                      0))
              (setf (music-state audio-player) :stopped)
              (play-music audio-player (test-resource-path "silence.wav") :num-plays 2)
              (loop while (eq :playing (music-state audio-player))
                 for timeout from 0 to 10 do
                   (sleep 1))
              (is (car audio-data) 998 "Callback timer reset for next song." :test #'<=)
              (quit)
              (make-instance 'test-scene :width 10 :height 10))))))
