(in-package :recurse.vert)

(defparameter %test-thing-pub-body-invokes% (list))
(defparameter %test-thing-sub-body-invokes% (list))
(defparameter %test-thing-global-invokes% (list))

(defclass test-pub (event-publisher) ())

(defclass test-sub () ())

(defevent test-thing (pub arg1 arg2)
    "A test event"
  (push (list pub arg1 arg2)
        %test-thing-pub-body-invokes%))

(defevent-handler test-thing ((pub test-pub) (sub test-sub) arg1 arg2)
    "Some doc"
  (push (list pub sub arg1 arg2)
        %test-thing-sub-body-invokes%))

(defevent-handler-global global-test-thing (test-thing pub arg1 arg2)
    "Some doc"
  (push (list pub arg1 arg2)
        %test-thing-global-invokes%))

(prove:deftest test-events-do-not-cons
  (block warm-up
    (loop :for i :from 1 :below 1024 :do
      (event-publish 'input-happened
                     *standard-input*
                     'arg-one
                     'arg-two
                     'arg-three
                     'arg-four
                     'arg-five)
          :finally
             (events-run-pending)
             (events-run-pending))
    (log:config :off)
    (log4cl:flush-all-appenders)
    (garbage-collect-block)
    (log:config :info))

  ;; A healthy, object-pooled event system allocates *nothing* during
  ;; steady-state publish/run-pending. We assert this per-frame rather than as a
  ;; single start-to-finish delta because:
  ;;   1. sb-ext:get-bytes-consed is process-wide, so background activity from
  ;;      other subsystems can sporadically inflate a whole-run delta and make a
  ;;      sub-cons-cell threshold flaky.
  ;;   2. The regression we care about (mismatched %pending-events% / %event-bus%
  ;;      capacities being ping-ponged by the rotatef swap in EVENTS-RUN-PENDING)
  ;;      reallocates a large vector on *every* frame -- so a real regression
  ;;      shows up as hundreds of allocating frames, while incidental external
  ;;      noise touches at most a handful.
  ;; So we count how many frames allocated at all and require that to be tiny.
  (loop :with pub-size = 256
        :and num-frames = 0
        :and num-allocating-frames = 0
        :and max-frame-bytes = 0
        ;; the bug reallocated ~32KB every frame; allow a tiny slack for rare
        ;; process-wide noise from other threads/finalizers/GC bookkeeping.
        :and max-allocating-frames = 4
        :for i :from 1 :below 300000 :do
          (event-publish 'input-happened
                         *standard-input*
                         'arg-one
                         'arg-two
                         'arg-three
                         'arg-four
                         'arg-five)
          (when (and (> i 0) (= (mod i pub-size) 0))
            (assert (= (length %pending-events%) pub-size))
            (let ((before (sb-ext:get-bytes-consed)))
              (events-run-pending)
              (let ((frame-bytes (- (sb-ext:get-bytes-consed) before)))
                (incf num-frames)
                (when (> frame-bytes 0)
                  (incf num-allocating-frames)
                  (when (> frame-bytes max-frame-bytes)
                    (setf max-frame-bytes frame-bytes)))))
            (assert (= (length %pending-events%) 0)))
        :finally
           (prove:is num-allocating-frames
                     max-allocating-frames
                     :test #'<=
                     (format nil "Event publish/run-pending does not allocate per-frame (~A/~A frames allocated, max ~A bytes)"
                             num-allocating-frames num-frames max-frame-bytes))))

(prove:deftest test-events-buffers-do-not-grow-each-frame
  "Regression test: EVENTS-RUN-PENDING swaps %pending-events% and %event-bus%
every frame via ROTATEF. If the two buffers ever have different capacities, the
smaller one gets reallocated every time it is filled, and -- because the swap
ping-pongs them -- this reallocation recurs on every single frame forever. This
manifested in-game as ~10x the expected GC pressure. Here we publish frames of
*varying* size (which is what a real game does) and assert the backing buffers
reach a steady capacity and stop growing."
  ;; warm the buffers up to a high-water mark with one large frame
  (loop :for i :from 1 :below 2048 :do
    (event-publish 'input-happened *standard-input* 'a 'b 'c 'd 'e))
  (events-run-pending)
  (events-run-pending)

  (let ((settled-pending-cap (array-dimension %pending-events% 0))
        (settled-bus-cap (array-dimension %event-bus% 0))
        (grow-events 0))
    ;; Now run many frames of differing sizes. With the bug, the smaller buffer
    ;; is regrown on (nearly) every frame.
    (loop :for frame :from 0 :below 500 :do
      (loop :for i :from 0 :below (+ 16 (mod frame 200)) :do
        (event-publish 'input-happened *standard-input* 'a 'b 'c 'd 'e))
      (events-run-pending)
      (when (or (> (array-dimension %pending-events% 0) settled-pending-cap)
                (> (array-dimension %event-bus% 0) settled-bus-cap))
        (incf grow-events)
        (setf settled-pending-cap (max settled-pending-cap
                                       (array-dimension %pending-events% 0))
              settled-bus-cap (max settled-bus-cap
                                   (array-dimension %event-bus% 0)))))
    (prove:is grow-events
              0
              "Event buffers reach a steady capacity and stop reallocating each frame")))

(prove:deftest test-events-low-level
  (events-run-pending)

  (let ((%test-thing-pub-body-invokes% (list))
        (%test-thing-sub-body-invokes% (list))
        (%test-thing-global-invokes% (list))
        (sub1 (make-instance 'test-sub))
        (sub2 (make-instance 'test-sub))
        (sub3 (make-instance 'test-sub))
        (pub (make-instance 'test-pub))
        (num-events 100))
    (event-subscribe pub sub1 test-thing)
    (event-subscribe pub sub2 test-thing)
    (event-subscribe pub sub3 test-thing some-other-event)
    (prove:is (slot-value pub 'event-subscribers)
              (%make-hash (list 'test-thing (vector sub1 sub2 sub3)
                                'some-other-event (vector sub3)))
              :test #'%hash-tables-equalp
              "All subs in subscriber list")
    (event-unsubscribe pub sub3 test-thing)
    (prove:is (slot-value pub 'event-subscribers)
              (%make-hash (list 'test-thing (vector sub1 sub2)
                                'some-other-event (vector sub3)))
              :test #'%hash-tables-equalp
              "sub3 removed from test-thing event")
    ;; (event-subscribe pub sub3 test-thing some-other-event)
    ;; (event-unsubscribe pub sub3 test-thing)
    (loop :for i :from 0 :below num-events :do
      (event-publish 'test-thing pub 1 2))
    (events-run-pending)

    (prove:is (length %test-thing-pub-body-invokes%) 100)
    (prove:is (first %test-thing-pub-body-invokes%)
              (list pub 1 2)
              :test #'equalp)
    (prove:is %test-thing-pub-body-invokes%
              (make-list num-events
                         :initial-element
                         (list pub 1 2))
              :test #'equalp)
    (prove:is (length %test-thing-sub-body-invokes%) (* 2 num-events)
              "Each subscribed sub receives event handler callback.")

    ;; expecting the sub invoke list to alternate between sub1 and sub2
    (loop :with expected-sub = (second (first %test-thing-sub-body-invokes%))
          :for i :from 0
          :for invoke :in  %test-thing-sub-body-invokes% :do
            (unless (eq (second invoke) expected-sub)
              (prove:fail
               (format nil "~A :: (1: ~A, 2: ~A) expected to find sub ~A, but found ~A instead"
                       i sub1 sub2
                       expected-sub
                       (second invoke)))
              (return))
            (log:trace "~A (1: ~A, 2: ~A) Found expected sub: ~A"
                       i sub1 sub2
                       expected-sub)
            (if (eq sub1 expected-sub)
                (setf expected-sub sub2)
                (setf expected-sub sub1))
          :finally
             (prove:pass "Subs invoked in expected order."))

    (prove:is %test-thing-global-invokes% %test-thing-pub-body-invokes%
              :test #'equalp
              ;; global handler body produces the same list structure as the pub list
              "Global handler invoked")))

(defun %make-hash (plist)
  "Make a hash table out of PLIST"
  (let ((h (make-hash-table :test #'eq)))
    (loop :for (k v) :on plist :by #'cddr :while v :do
      (setf (gethash k h) v))
    h))

(defun %hash-tables-equalp (hash1 hash2)
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (loop :for key1 :being :the hash-keys :of hash1
               :using (hash-value value1) :do
                 ;; remove impl null padding
                 (let ((value1 (remove-if #'null value1))
                       (value2 (remove-if #'null (gethash key1 hash2 #()))))
                   (unless (and (= (length value1)
                                   (length value2))
                                (loop :for v1 :across value1
                                      :for v2 :across value2 :do
                                        (unless (equalp v1 v2)
                                          (return nil))
                                      :finally
                                         (return t)))
                     (return nil)))
             :finally
                (return t))))
