;; Implement audio-player with sdl2-mixer
(in-package :recurse.vert)

;;;; sdl constants

;; TODO make params constants
(defconstant +sdl-mixer-audio-device-id+ 2
  "SDL_AudioDeviceID which references out sdl-mixer. Hardcoded because we will only ever use one sdl-mixer and no other sdl audio device.")

(defparameter *first-sfx-channel* 0)
(defparameter *num-sfx-channels* 20)
(defconstant +first-free-channel+ -1)
(defconstant +all-channels+ -1)
(defconstant +music-channel+ -2)

(defparameter *sdl-mix-sample-size* 0 "Number of 8 bit ints in a sample.
Computed as (* (/ bit-rate 8) num-channels)")
(defparameter *sdl-mix-rate* 0 "Frequency sdl is mixing.")

;;;; audio-sample and audio-state

@export-class
(defclass audio-sample ()
  ((path-to-audio :initarg :path-to-audio
                  :reader audio-sample-path-to-audio
                  :initform (error ":path-to-audio required"))
   (type :initarg :type
         :initform (error ":type must be :music or :sfx")
         :documentation "specify the audio as a sfx or music sample.")
   (sdl-buffer :initform nil
               :documentation "pointer to sdl ffi bits")
   (volume :initarg :volume
           :initform sdl2-ffi:+MIX-MAX-VOLUME+
           :reader audio-sample-volume)
   (audio-sample-releaser :initform nil))
  (:documentation "An audio sample (usually a wrapper around an audio file, like a wav). May be music or sfx."))

(defmethod initialize-instance :around ((sample audio-sample) &rest args)
  (declare (optimize (speed 3)))
  (let ((all-args (cons sample (copy-list args))))
    (declare (dynamic-extent all-args))
    (prog1 (apply #'call-next-method all-args)
      (resource-autoloader-add-object *resource-autoloader*
                                      (tg:make-weak-pointer sample)))))

(defmethod initialize-instance :after ((sample audio-sample) &rest args)
  (declare (ignore args))
  (ecase (slot-value sample 'type)
    ;; valid type keywords
    (:music)
    (:sfx)))

(defun %release-audio-sample-resources (buffer-type sdl-buffer)
  (ecase buffer-type
    (:music
     (sdl2-mixer:free-music sdl-buffer))
    (:sfx
     (sdl2-mixer:free-chunk sdl-buffer))))

(defmethod load-resources ((sample audio-sample))
  (prog1 (call-next-method sample)
    (unless (slot-value sample 'audio-sample-releaser)
      (let* ((buffer-type (slot-value sample 'type))
             (sdl-buffer (ecase buffer-type
                           (:music
                            (sdl2-mixer:load-music (slot-value sample 'path-to-audio)))
                           (:sfx
                            (let ((chunk (sdl2-mixer:load-wav (slot-value sample 'path-to-audio))))
                              (log:trace "allocated chunk: ~A (vol = ~A)"
                                         (audio-sample-path-to-audio sample)
                                         (audio-sample-volume sample))
                              (sdl2-ffi.functions:mix-volume-chunk chunk (audio-sample-volume sample))
                              chunk)))))
        (setf (slot-value sample 'sdl-buffer) sdl-buffer
              (slot-value sample 'audio-sample-releaser)
              (make-resource-releaser (sample)
                (%release-audio-sample-resources buffer-type sdl-buffer)))))))

(defmethod release-resources ((sample audio-sample))
  (prog1 (call-next-method sample)
    (with-slots (audio-sample-releaser type sdl-buffer) sample
      (when audio-sample-releaser
        (%release-audio-sample-resources type sdl-buffer)
        (cancel-resource-releaser audio-sample-releaser)
        (setf audio-sample-releaser nil)))))

(defun %audio-samples-equalp (sample1 sample2)
  (declare (audio-sample sample1 sample2))
  (with-slots ((path-to-audio1 path-to-audio)
               (type1 type)
               (sdl-buffer1 sdl-buffer)
               (volume1 volume))
      sample1
  (with-slots ((path-to-audio2 path-to-audio)
               (type2 type)
               (sdl-buffer2 sdl-buffer)
               (volume2 volume))
      sample2
    (and (equalp path-to-audio1 path-to-audio2)
         (equal type1 type2)
         (eq sdl-buffer1 sdl-buffer2)
         (equalp volume1 volume2)))))

(defclass sdl-channel ()
  ((sample :initarg :sample
           :initform nil
           :accessor sdl-channel-sample
           :documentation "The sample playing on this channel.")
   (channel-number :initarg :channel-number
                   :accessor sdl-channel-number
                   :initform (error ":channel-number required"))
   (channel-volume :initarg :channel-volume
                   :initform sdl2-ffi:+MIX-MAX-VOLUME+
                   :accessor sdl-channel-volume)
   (start-time-samples :initarg :start-time-samples
                       :initform nil
                       :accessor sdl-channel-start-time-samples
                       :documentation "sample timestamp when this channel started playing."))
  (:documentation "Represents one SDL channel."))

(defun %sdl-channel-copy (src-channel dest-channel)
  "Copy contents of SRC-CHANNEL into DEST-CHANNEL"
  (declare (sdl-channel src-channel dest-channel))
  (setf (sdl-channel-sample dest-channel) (sdl-channel-sample src-channel)
        (sdl-channel-number dest-channel) (sdl-channel-number src-channel)
        (sdl-channel-volume dest-channel) (sdl-channel-volume src-channel)
        (sdl-channel-start-time-samples dest-channel) (sdl-channel-start-time-samples src-channel))
  dest-channel)

(defun %sdl-channels-equal-p (channel1 channel2)
  (and (or (and (null (sdl-channel-sample channel1))
                (null (sdl-channel-sample channel2)))
           (and (sdl-channel-sample channel1)
                (sdl-channel-sample channel2)))
       (or (and (null (sdl-channel-sample channel1)) (null (sdl-channel-sample channel2)))
           (and (not (null (sdl-channel-sample channel1))) (not (null (sdl-channel-sample channel2)))
                (%audio-samples-equalp (sdl-channel-sample channel1) (sdl-channel-sample channel2))))
       (equalp (sdl-channel-volume channel1) (sdl-channel-volume channel2))
       (equalp (sdl-channel-number channel1) (sdl-channel-number channel2))
       (equalp (sdl-channel-start-time-samples channel1) (sdl-channel-start-time-samples channel2))))

@export-class
(defclass audio-state ()
  ((current-time-samples :initform 0
                         :accessor audio-state-current-time-samples
                         :type fixnum
                         :documentation "How many samples this audio-state has processed. audio-player advances the sample time for its active audio-state.")
   (music-channel :initform
                  (make-instance 'sdl-channel
                                 :channel-number +music-channel+)
                  :accessor audio-state-music-channel)
   (sfx-channels :initform (make-array 0
                                       :adjustable t
                                       :fill-pointer 0)
                 :reader audio-state-sfx-channels
                 :documentation "Array of SDL-SFX-CHANNEL objects. Ordered by channel number")
   (sfx-volume :initarg :sfx-volume
               :initform
               (if *config*
                   (getconfig 'audio-player-sfx-volume *config*)
                   1.0)
               :accessor audio-state-sfx-volume)
   (music-volume :initarg :music-volume
                 :initform
                 (if *config*
                     (getconfig 'audio-player-music-volume *config*)
                     1.0)
                 :accessor audio-state-music-volume))
  (:documentation "The state of all audio. One channel of music and up to 8 channels of sound effects"))

(defgeneric audio-state-music-state (audio-state)
  (:documentation "Returns :playing :paused or :stopped depending on the state of the music.")
  (:method ((audio-state audio-state))
    (if (audio-state-music-channel audio-state)
        :playing
        :stopped)))

;;;; sdl audio player

(defclass sdl-audio-player (audio-player)
  ((audio-state :initform (make-instance 'audio-state)
                :documentation "The active audio-state being processed. The audio-player will update this audio-state as samples are processed.
For thread safety, this slot should not be directly accessed outside of audio-player internals. See AUDIO-PLAYER-COPY-STATE and AUDIO-PLAYER-LOAD-STATE if you wish to alter or read the audio state.")
   (channel-pool :initform (make-array 0
                                       :element-type 'sdl-channel
                                       :fill-pointer 0
                                       :adjustable t)))
  (:documentation "audio-player implemented with sdl-mixer."))

(defun sdl-audio-player-get-channel (sdl-audio-player)
  "Get a new sdl-channel out of SDL-AUDIO-PLAYER's object pool, or create a fresh one if the pool is empty."
  (with-slots (channel-pool) sdl-audio-player
    (declare (vector channel-pool))
    (if (> (length channel-pool) 0)
        (vector-pop channel-pool)
        (make-instance 'sdl-channel
                       :channel-number 0))))

(defun sdl-audio-player-return-channel (sdl-audio-player sdl-channel)
  "return SDL-CHANNEL to SDL-AUDIO-PLAYER's object pool."
  (with-slots (channel-pool) sdl-audio-player
    (unless (find sdl-channel channel-pool :test #'eq)
      (vector-push-extend
       sdl-channel
       channel-pool))
    sdl-audio-player))

@export
(defmacro with-sdl-mixer-lock-held (&body body)
  "Run BODY with sdl-mixer's audio device locked. No audio callback fns will run during body.
Don't block this thread on any audio callbacks or else a deadlock will occur."
  `(unwind-protect
        (progn
          (sdl2-ffi.functions:sdl-lock-audio-device +sdl-mixer-audio-device-id+)
          ,@body)
     (sdl2-ffi.functions:sdl-unlock-audio-device +sdl-mixer-audio-device-id+)))

@export
(defun convert-audio-samples->ms (audio-samples)
  (declare (optimize (speed 3))
           (fixnum audio-samples))
  (/ audio-samples
     #.(/ +output-frequency-hz+ 1000.0)))

@export
(defun convert-ms->audio-sample (ms)
  (declare (optimize (speed 3))
           (single-float ms))
  (round (* ms #.(/ +output-frequency-hz+ 1000.0))))

;; audio-state copy implementations

(defmethod audio-player-copy-state ((audio-player sdl-audio-player) &optional (destination-audio-state (make-instance 'audio-state)))
  (with-slots (audio-state) audio-player
    (declare (audio-state audio-state destination-audio-state))
    (with-sdl-mixer-lock-held
      #+nil ; print sfx channel info for an audio state
      (flet ((log-state-channel-info (state)
               (with-slots ((channels vert::sfx-channels)) state
                 (loop :for chan :across channels :do
                   (with-slots ((num vert::channel-number) (sample vert::sample)) chan
                     (format t "~A -> ~A~%" num (when sample
                                                  (slot-value sample 'vert::path-to-audio))))))))
        (log:info "-- copy state SRC --")
        (log-state-channel-info audio-state)
        (break))
      (with-slots ((current-time current-time-samples)
                   (current-music-channel music-channel)
                   (current-sfx-channels sfx-channels)
                   (current-music-volume music-volume)
                   (current-sfx-volume sfx-volume))
          audio-state
        (with-slots ((dest-time current-time-samples)
                     (dest-music-channel music-channel)
                     (dest-sfx-channels sfx-channels)
                     (dest-music-volume music-volume)
                     (dest-sfx-volume sfx-volume))
            destination-audio-state
          (setf dest-time current-time
                dest-music-volume current-music-volume
                dest-sfx-volume current-sfx-volume)
          (%sdl-channel-copy current-music-channel dest-music-channel)
          (loop :for i :from 0
             :for current-channel :across current-sfx-channels :do
               (when (>= i (length dest-sfx-channels))
                 (vector-push-extend (sdl-audio-player-get-channel audio-player)
                                     dest-sfx-channels))
               (%sdl-channel-copy current-channel (elt dest-sfx-channels i))
             :finally
               (setf (fill-pointer dest-sfx-channels) i))))
      destination-audio-state)))

(defmethod audio-player-load-state ((audio-player sdl-audio-player) (new-audio-state audio-state))
  (with-slots (audio-state) audio-player
    (declare (audio-state audio-state new-audio-state))
    (with-sdl-mixer-lock-held
      (with-slots ((current-time current-time-samples)
                   (current-music-channel music-channel)
                   (current-sfx-channels sfx-channels))
          audio-state
        (with-slots ((new-time current-time-samples)
                     (new-music-channel music-channel)
                     (new-sfx-channels sfx-channels))
            new-audio-state
          (sdl-audio-player-load-music-state audio-player new-audio-state)
          (sdl-audio-player-load-sfx-state audio-player new-audio-state)
          (setf current-time new-time))
        new-audio-state))))

@export
(defun sdl-audio-player-load-music-state (audio-player new-audio-state)
  "Load music info from NEW-AUDIO-STATE into AUDIO-PLAYER's internal state. Must be called under AUDIO-PLAYER's lock."
  (with-slots (audio-state) audio-player
    (declare (audio-state audio-state new-audio-state))
    (with-slots ((current-music-channel music-channel)
                 (current-music-volume music-volume))
        audio-state
      (with-slots ((new-music-channel music-channel)
                   (new-music-volume music-volume))
          new-audio-state
        (let ((new-music-p (not (%sdl-channels-equal-p current-music-channel new-music-channel))))
          (when new-music-p
            (%sdl-channel-copy new-music-channel current-music-channel)
            (sdl2-mixer:halt-music)
            (when (sdl-channel-sample new-music-channel)
              (unless (= 0 (sdl2-mixer:play-music (slot-value (sdl-channel-sample new-music-channel) 'sdl-buffer) 1))
                (error "sdl-mixer unable to play music: ~A"
                       (sdl2-ffi.functions:sdl-get-error)))
              (if (sdl-channel-start-time-samples new-music-channel)
                  ;; resume music playback
                  (log:warn "FIXME Music resume not yet supported for WAV music. Will need a similar hack to SFX resume. Please fix this!")
                  #+nil
                  (let ((music-position-seconds (/ (convert-audio-samples->ms
                                                    (- (audio-state-current-time-samples new-audio-state)
                                                       (sdl-channel-start-time-samples new-music-channel)))
                                                   1000.0)))
                    (log:debug "Playing music [~A] at position ~A seconds"
                               (audio-sample-path-to-audio (sdl-channel-sample current-music-channel))
                               music-position-seconds)
                    (log:debug "Resuming music playback at ~A seconds" music-position-seconds)
                    ;; Note: mixer position setting differs for different file types
                    ;; https://www.libsdl.org/projects/SDL_mixer/docs/SDL_mixer.html#SEC65
                    (sdl2-ffi.functions:mix-set-music-position
                     ;; TODO: consing due to double-float boxing
                     (coerce music-position-seconds
                             'double-float)))
                  ;; music start playing for the first time
                  (progn
                    (log:debug "New music playing: ~A"
                               (audio-sample-path-to-audio (sdl-channel-sample current-music-channel)))
                    (setf (sdl-channel-start-time-samples current-music-channel)
                          (audio-state-current-time-samples new-audio-state))))
              (sdl2-ffi.functions:mix-pause-music)))
          ;; note: resume is safe no matter what the state of the music is
          (setf current-music-volume new-music-volume)
          (when *audio*
            (sdl2-ffi.functions:mix-volume-music
             (floor (* new-music-volume (sdl-channel-volume new-music-channel))))
            (sdl2-ffi.functions:mix-resume-music)))))))

@export
(defun sdl-audio-player-load-sfx-state (audio-player new-audio-state)
  "Load sfx info from NEW-AUDIO-STATE into AUDIO-PLAYER's internal state. Must be called under AUDIO-PLAYER's lock."
  (with-slots (audio-state) audio-player
    (declare (audio-state audio-state new-audio-state))
    (with-slots ((current-sfx-channels sfx-channels)
                 (current-sfx-volume sfx-volume))
        audio-state
      (with-slots ((new-sfx-channels sfx-channels)
                   (new-sfx-volume sfx-volume))
          new-audio-state
        (block add-new-channels
          (loop :while (< (length current-sfx-channels)
                          (length new-sfx-channels))
                :do
                   (vector-push-extend (sdl-audio-player-get-channel audio-player)
                                       current-sfx-channels)
                   (log:debug "Added new sfx channel for state change: ~A"
                              (length current-sfx-channels))))
        (block remove-extra-channels
          (when (< (length new-sfx-channels)
                   (length current-sfx-channels))
            (log:debug "state change truncate existing mixer channels: ~A -> ~A"
                       (length current-sfx-channels)
                       (length new-sfx-channels))
            (loop :for i :from (length new-sfx-channels) :below (length current-sfx-channels)
                  :do
                     (log:debug "-- halting channel ~A" i)
                     (sdl2-mixer:halt-channel
                      (sdl-channel-number (elt current-sfx-channels i)))
                     ;; return to object pool
                     (sdl-audio-player-return-channel audio-player (elt current-sfx-channels i)))
            (setf (fill-pointer current-sfx-channels)
                  (length new-sfx-channels))))
        (block copy-existing-channels
          (loop :for i :from 0
                :for new-channel :across new-sfx-channels :do
                  (let ((current-channel (elt current-sfx-channels i)))
                    (unless (%sdl-channels-equal-p new-channel current-channel)
                      ;; be mindful that calling HALT-CHANNEL will invoke %%CHANNEL-FINISHED-CALLBACK
                      (sdl2-mixer:halt-channel (sdl-channel-number new-channel))
                      (%sdl-channel-copy new-channel current-channel)

                      (when (sdl-channel-sample new-channel)
                        (sdl2-ffi.functions:mix-play-channel-timed
                         (sdl-channel-number new-channel)
                         (slot-value (sdl-channel-sample new-channel) 'sdl-buffer)
                         ;; hardcoding a one-time play
                         0 -1)
                        (sdl2-ffi.functions:mix-pause (sdl-channel-number new-channel))
                        (log:debug "queuing sfx play on mixer channel: ~A -> ~A"
                                   (sdl-channel-number new-channel)
                                   (audio-sample-path-to-audio (sdl-channel-sample new-channel)))

                        (if (and (sdl-channel-start-time-samples current-channel)
                                 (/= (sdl-channel-start-time-samples current-channel) (audio-state-current-time-samples new-audio-state)))
                            (let ((sfx-position-samples (- (audio-state-current-time-samples new-audio-state)
                                                           (sdl-channel-start-time-samples current-channel))))
                              (when (< sfx-position-samples 0)
                                (log:warn "Invalid start time for sdl channel: ~A -- ~A. Setting to zero."
                                          (sdl-channel-number current-channel)
                                          (audio-sample-path-to-audio (sdl-channel-sample current-channel)))
                                (setf sfx-position-samples 0))
                              (when *audio*
                                (log:debug "Seek to position ~Ams for channel ~A"
                                           (convert-audio-samples->ms sfx-position-samples)
                                           (sdl-channel-number new-channel))
                                (%%seek-sdl-mix-chunk (slot-value (sdl-channel-sample new-channel) 'sdl-buffer)
                                                      sfx-position-samples
                                                      (lambda ()
                                                        (sdl2-ffi.functions:mix-play-channel-timed
                                                         (sdl-channel-number new-channel)
                                                         (slot-value (sdl-channel-sample new-channel) 'sdl-buffer)
                                                         ;; hardcoding a one-time play
                                                         0 -1)
                                                        (sdl2-ffi.functions:mix-pause (sdl-channel-number new-channel))))
                                ;; need to copy and update state again because channel position change will invoke sdl mixer callbacks
                                (%sdl-channel-copy new-channel current-channel)
                                (setf (sdl-channel-start-time-samples current-channel) (- (audio-state-current-time-samples new-audio-state) sfx-position-samples))))
                            ;; resume channel playback channel began playing. mark start time
                            (setf (sdl-channel-start-time-samples current-channel) (audio-state-current-time-samples new-audio-state)))
                        (when *audio*
                          (sdl2-ffi.functions:mix-volume
                           (sdl-channel-number current-channel)
                           (floor (* new-sfx-volume (sdl-channel-volume current-channel))))))))))
        (setf current-sfx-volume new-sfx-volume)

        (when *audio*
          (sdl2-ffi.functions:mix-resume +all-channels+))))))

(defun %%seek-sdl-mix-chunk (sdl-buffer position-samples fn)
  "Seek SDL-BUFFER to POSITION-SAMPLES and invoke FN. Buffer will be reset after this fn completes."
  (declare (fixnum position-samples)
           ((function ()) fn))
  ;; https://stackoverflow.com/questions/14691530/sdl-mixer-set-sound-position
  ;; https://github.com/aduros/SDL_mixer/blob/ac2df1b04a424c507839303ffb1c9cf000a95ac3/mixer.c#L815
  ;; from sdl mixer header
  ;; typedef struct Mix_Chunk {
  ;; 	int allocated;
  ;; 	Uint8 *abuf;
  ;; 	Uint32 alen;
  ;; 	Uint8 volume;		/* Per-sample volume, 0-128 */
  ;; } Mix_Chunk;
  ;; seeking for sound effect channels is not officially supported.
  ;; as a hack, we'll modify the Mix_Chunk struct
  ;;  - inc the array starting position
  ;;  - dec the array length
  ;; FNs like play-channel will copy the modified buffer when they are invoked (i.e. during the FN funcall)
  ;; finally we'll reset the ffi values to their original settings
  (cffi:with-foreign-slots ((allocated abuf alen volume)
                            (slot-value sdl-buffer 'autowrap::ptr)
                            (:struct my-mix-chunk))
    (let ((foreign-offset-bytes (* position-samples
                                   ;; multiply by 4. Array is uint8. 8 -> 16 (1 samp), and 16 -> 32 (LR output)
                                   #.(+ (/ +output-bit-depth+ 8) +output-num-channels+)
                                   (cffi:foreign-type-size :uint8)))
          (reset-pointer-p nil)
          (reset-len-p nil))
      (log:trace "sfx pointer moved -> ~A" foreign-offset-bytes)
      (unwind-protect
           (progn
             (cffi:incf-pointer abuf foreign-offset-bytes)
             (setf reset-pointer-p t
                   alen (- alen foreign-offset-bytes)
                   reset-len-p t)
             (funcall fn))
        (when reset-pointer-p
          (cffi:incf-pointer abuf (- foreign-offset-bytes)))
        (when reset-len-p
          (setf alen
                (min (max 0 (+ alen foreign-offset-bytes))
                     #.(- (expt 2 32) 1))))))))

;; audio sound loading implementation

(defvar *sdl-audio-samples-cache*
  (getcache-default "sdl-audio-samples"
                    *engine-caches*
                    (make-instance 'counting-cache
                                   :on-evict
                                   (lambda (path-to-audio audio-sample)
                                     (declare (ignore path-to-audio))
                                     (release-resources audio-sample))))
  "Cache of sdl-mixer music objects")

(defvar *legacy-sdl-audio-cache*
  (getcache-default "legacy-sdl-cache"
                    *engine-caches*
                    (make-instance 'evict-oldest-cache :test #'equalp
                                   :max-size 50
                                   :on-evict (lambda (path-to-file sample)
                                               (declare (ignore path-to-file))
                                               (release-resources sample))))
  "Time-based sdl cache.
Long term plan is to cache audio samples in the game-objects or scenes which need the audio using the *SDL-AUDIO-SAMPLES-CACHE*.")

(defmethod audio-player-load-sfx ((audio-player sdl-audio-player) path-to-sfx &key (volume 1.0))
  (unless (and (> (length path-to-sfx) 4)
               (equalp ".wav"
                       (subseq path-to-sfx
                               (- (length path-to-sfx) 4))))
    (error "Only .wav file supported. Got: ~A" path-to-sfx))
  (getcache-default path-to-sfx
                    *legacy-sdl-audio-cache*
                    (make-instance 'audio-sample
                                   :type :sfx
                                   :volume (floor (* volume sdl2-ffi:+MIX-MAX-VOLUME+))
                                   :path-to-audio path-to-sfx)))

(defmethod audio-player-load-music ((audio-player sdl-audio-player) path-to-music)
  (unless (and (> (length path-to-music) 4)
               (equalp ".wav"
                       (subseq path-to-music
                               (- (length path-to-music) 4))))
    (error "Only .wav file supported. Got: ~A" path-to-music))
  (getcache-default path-to-music
                    *legacy-sdl-audio-cache*
                    (make-instance 'audio-sample
                                   :type :music
                                   :path-to-audio path-to-music)))

;;;; callbacks which run on sdl-mixer under the audio lock
(defun %%music-finished-callback ()
  (declare (optimize (speed 3)))
  (let ((audio *audio*))
    (when audio
      (with-slots (audio-state) audio
        (with-slots (music-channel) audio-state
          (when (sdl-channel-sample music-channel)
            (log:debug "Reached end of song (~A). Looping back to beginning."
                       (audio-sample-path-to-audio (sdl-channel-sample music-channel)))
            (unless (= 0 (the fixnum (sdl2-mixer:play-music (slot-value (sdl-channel-sample music-channel) 'sdl-buffer) 1)))
              (log:error "sdl-mixer unable to play music: ~A"
                         (sdl2-ffi.functions:sdl-get-error)))
            (setf (sdl-channel-start-time-samples music-channel)
                  ;; looping is hardcoded.
                  ;; This means we're just starting to play the song again.
                  (audio-state-current-time-samples audio-state))))))))

(defun %%postmix-callback (udata stream len)
  "Note: This fn runs on the sdl-mixer audio thread."
  (declare (optimize (speed 3))
           (ignore udata stream)
           (fixnum len))
  ;; Assuming hardcoded spec is in effect.
  ;; STREAM is an array of int8 of size LEN
  ;; 32 bits over each sample (or 4 unit8)
  ;; 0-1 == Left channel
  ;; 2-3 == Right channel
  ;; Check music format to find LSB/MSB
  (let ((audio *audio*))
    (when audio
      (with-slots (audio-state) audio
        (with-slots (current-time-samples) audio-state
          (declare (fixnum current-time-samples))
          (setf current-time-samples
                (+ current-time-samples
                   ;;  ;; divide by 4 because LEN is for 8bit array but sample format is 16 bit audio 16. Divide by 2.
                   ;;  ;; and the sample array is for two channels (left and right speakers). Divide by 2 again.
                   (the fixnum (/ len 4))))
          (log:trace "audio thread tick: ~A (~Ams). ~A delta (~A ms)"
                     (audio-state-current-time-samples audio-state)
                     (convert-audio-samples->ms
                      (audio-state-current-time-samples audio-state))
                     (the fixnum (/ len 4))
                     (convert-audio-samples->ms
                      (the fixnum (/ len 4)))))))))

(defun %%channel-finished-callback (channel-number)
  (declare (optimize (speed 3))
           (fixnum channel-number))
  (let ((audio *audio*))
    (when audio
      (with-slots (audio-state) audio
        (with-slots (sfx-channels) audio-state
          (declare ((vector sdl-channel) sfx-channels))
          (loop :for sdl-channel :across sfx-channels :do
               (when (equalp (the fixnum (sdl-channel-number sdl-channel))
                             channel-number)
                 (setf (sdl-channel-sample sdl-channel) nil
                       (sdl-channel-start-time-samples sdl-channel) nil)
                 (return))
             :finally
               (log:debug "Unable to find sdl-channel in audio-state for channel: ~A. This is can happen when a new audio-state is loaded while a channel is playing."
                          channel-number)))))))

(defun enable-mixer-callbacks ()
  "Add callbacks to sdl-mixer to hook various audio events."

  ;; needed for sfx seeking
  (cffi:defcstruct my-mix-chunk
    (allocated :int)
    (abuf (:pointer :uint8))
    (alen :uint32)
    (volume :uint8))

  ;;first define the callbacks
  (cffi:defcallback music-finished-callback :void ()
    (%%music-finished-callback))
  (cffi:defcallback postmix-callback :void ((udata :pointer) (stream :pointer) (len :int))
    (%%postmix-callback udata stream len))
  (cffi:defcallback channel-finished-callback :void ((channel-number :int))
    (%%channel-finished-callback channel-number))

  ;; now send the callbacks to sdl-mixer
  (with-sdl-mixer-lock-held
    (sdl2-ffi.functions:mix-hook-music-finished
     (cffi:callback music-finished-callback))
    ;; second arg, udata, is not used so we'll pass nil.
    (sdl2-ffi.functions:mix-set-post-mix (cffi:callback postmix-callback)
                                         nil)
    (sdl2-ffi.functions:mix-channel-finished
     (cffi:callback channel-finished-callback))))

(defun disable-mixer-callbacks ()
  (with-sdl-mixer-lock-held
    (sdl2-ffi.functions:mix-hook-music-finished
     (cffi:null-pointer))
    (sdl2-ffi.functions:mix-set-post-mix (cffi:null-pointer)
                                         nil)))

;;;; audio player api implementation

(defmethod start-audio-player ((audio-player sdl-audio-player))
  (sdl2-mixer:init)
  (sdl2-mixer:open-audio +output-frequency-hz+ :s16sys 2 2048)
  (sdl2-mixer:allocate-channels *num-sfx-channels*)
  (sdl2-mixer:volume-music 128)
  (loop :for n :from *first-sfx-channel* :below *num-sfx-channels* :do
       (sdl2-mixer:volume n 64))

  (multiple-value-bind (frequency format channels)
      (plus-c:c-with ((freq :int)
                      (fmt sdl2-ffi:uint16)
                      (chans :int))
        (sdl2-ffi.functions:mix-query-spec (freq plus-c:&) (fmt plus-c:&) (chans plus-c:&))
        (values freq fmt chans))
    (let ((bits (logand format #XFF)))
      (unless (= bits +output-bit-depth+)
        (error "unsupported audio bit-depth: ~A" bits))
      (unless (= channels +output-num-channels+)
        (error "unsupported number of audio channels: ~A" channels))
      (unless (= +output-frequency-hz+ frequency)
        (error "unsupported frequency: ~A" frequency))
      (setf *sdl-mix-sample-size* (* (/ bits 8) channels))
      (setf *sdl-mix-rate* frequency)
      (log:info "started sdl-mixer. frequency = ~A, bit depth = ~A, num-channels= ~A"
                frequency
                bits
                channels)))
  (enable-mixer-callbacks)
  (values))

(defmethod stop-audio-player ((audio-player sdl-audio-player))
  (sdl2-mixer:halt-music)
  (sdl2-mixer:halt-channel +all-channels+)
  (disable-mixer-callbacks)
  (clear-cache *sdl-audio-samples-cache*)
  (clear-cache *legacy-sdl-audio-cache*)
  (sdl2-mixer:close-audio)
  (loop :while (/= 0 (sdl2-mixer:init 0)) :do
       (sdl2-mixer:quit))
  (values))

(let ((tmp-audio-state nil))
  (defmethod audio-player-play-sound-effect ((audio-player sdl-audio-player) path-to-sfx-file &key (rate 1.0) (volume 1.0))
    ;; TODO: volume above 1.0 will not have additional affect
    (declare (ignore rate)
             ((single-float 0.0 10.0) volume))
    (unless (getconfig 'use-dummy-audio-output *config*)
      (with-sdl-mixer-lock-held
        (with-slots (audio-state) audio-player
          (unless tmp-audio-state
            (setf tmp-audio-state
                  (audio-player-copy-state audio-player)))
          (audio-player-copy-state audio-player tmp-audio-state)
          (let ((channel (%get-or-create-free-sfx-channel audio-player tmp-audio-state)))
            (setf (sdl-channel-sample channel) (audio-player-load-sfx audio-player
                                                                      path-to-sfx-file
                                                                      :volume volume)
                  (sdl-channel-start-time-samples channel) nil))
          (audio-player-load-state audio-player tmp-audio-state)))
      audio-player)))

(defun %get-or-create-free-sfx-channel (audio-player audio-state)
  (loop :for channel :across (audio-state-sfx-channels audio-state) :do
    (when (null (sdl-channel-sample channel))
      ;; return the first free channel
      (return channel))
        :finally
           ;; no free channels. Get a new one out of the object pool.
           (let ((channel (sdl-audio-player-get-channel audio-player)))
             (setf (sdl-channel-number channel) (length (audio-state-sfx-channels audio-state)))
             (vector-push-extend channel
                                 (audio-state-sfx-channels audio-state))
             (return channel)))
  )

(let ((tmp-audio-state nil))
  (defmethod audio-player-play-music ((audio-player sdl-audio-player) path-to-music-file &key (num-plays -1))
    (unless (= -1 num-plays)
      (error "TODO"))
    (unless (getconfig 'use-dummy-audio-output *config*)
      (with-sdl-mixer-lock-held
        (with-slots (audio-state) audio-player
          (unless tmp-audio-state
            (setf tmp-audio-state
                  (audio-player-copy-state audio-player)))
          (audio-player-copy-state audio-player tmp-audio-state)
          (let ((channel (sdl-audio-player-get-channel audio-player)))
            (setf (sdl-channel-number channel) +music-channel+
                  (sdl-channel-sample channel) (audio-player-load-music audio-player
                                                                        path-to-music-file)
                  (sdl-channel-start-time-samples channel) nil
                  (audio-state-music-channel tmp-audio-state) channel))
          (audio-player-load-state audio-player tmp-audio-state)))
      audio-player)))

(let ((tmp-audio-state nil))
  (defmethod audio-player-stop-music ((audio-player sdl-audio-player))
    (with-sdl-mixer-lock-held
      (unless tmp-audio-state
        (setf tmp-audio-state
              (audio-player-copy-state audio-player)))
      (audio-player-copy-state audio-player tmp-audio-state)
      (let ((music (audio-state-music-channel tmp-audio-state)))
        (setf (sdl-channel-sample music) nil
              (sdl-channel-start-time-samples music) nil))
      (audio-player-load-state audio-player tmp-audio-state))
    audio-player))

(let ((tmp-audio-state nil))
  (defmethod audio-player-stop-sfx ((audio-player sdl-audio-player))
    (with-sdl-mixer-lock-held
      (unless tmp-audio-state
        (setf tmp-audio-state
              (audio-player-copy-state audio-player)))
      (audio-player-copy-state audio-player tmp-audio-state)
      (setf (fill-pointer (audio-state-sfx-channels tmp-audio-state)) 0)
      (audio-player-load-state audio-player tmp-audio-state))
    audio-player))

(defmethod audio-player-stop-all ((player sdl-audio-player))
  (with-sdl-mixer-lock-held
    (audio-player-stop-music player)
    (audio-player-stop-sfx player)))
