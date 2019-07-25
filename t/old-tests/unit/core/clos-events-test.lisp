(in-package :recurse.vert/test)

(deftest event-pub-sub
  "basic pub-sub"
  (let ((pub (make-instance 'test-event-publisher))
        (sub (make-instance 'test-event-subscriber))
        (simple-callback (event-callback-name "simple-event")))
    (fire-event pub simple-event) ; no effect on subscriber
    (add-subscriber pub sub simple-event)
    (fire-event pub simple-event)
    (prove:is (method-invoke-count sub simple-callback) 1
              "subscribe to simple event" :test #'=)
    (add-subscriber pub sub simple-event)
    (simple-event pub)
    (prove:is (method-invoke-count sub simple-callback) 2
              "add-subscriber is idempotent" :test #'=)
    (remove-subscriber pub sub simple-event)
    (remove-subscriber pub sub simple-event) ; should have no effect
    (simple-event pub)
    (prove:is (method-invoke-count sub simple-callback) 2
              "subscriber removed" :test #'=)))

(deftest event-sub-different-methods
  "two subs to different events"
  (let ((pub (make-instance 'test-event-publisher))
        (sub1 (make-instance 'test-event-subscriber))
        (sub2 (make-instance 'test-event-subscriber))
        (method-callback (event-callback-name "method-event"))
        (simple-callback (event-callback-name "simple-event")))
    (add-subscriber pub sub1 simple-event method-event)
    (add-subscriber pub sub2 simple-event)
    (fire-event pub simple-event)
    (prove:is (method-invoke-count sub1 simple-callback) 1
              "sub1 sees simple-event" :test #'=)
    (prove:is (method-invoke-count sub2 simple-callback) 1
              "sub2 sees simple-event" :test #'=)

    (fire-event pub method-event 4)
    (prove:is (method-invoke-count sub1 method-callback) 1
              "sub1 sees method-event" :test #'=)
    (prove:is (method-invoke-count sub2 method-callback) 0
              "sub2 does not see method-event" :test #'=)

    (remove-subscriber pub sub1 method-event)
    (fire-event pub method-event 4)
    (prove:is (method-invoke-count sub1 method-callback) 1
              "sub1 does not see method-event" :test #'=)
    (prove:is (method-invoke-count sub2 method-callback) 0
              "sub2 does not see method-event" :test #'=)))

(deftest event-hooking
  "before/after/around clos hooks for method event pub"
  (let ((pub (make-instance 'test-event-publisher))
        (sub (make-instance 'test-event-subscriber))
        (callback-name (event-callback-name "method-event")))
    (add-subscriber pub sub method-event)
    (is (fire-event pub method-event 3) 4
        "method-event body adds 1 to arg")
    (is (last-arg sub) 3
        "subscriber sees event arg"
        :test #'=)
    (is (method-invoke-order pub sub)
        (list "around:preamble:method-event"
              "before:method-event"
              "method-event"
              callback-name
              "after:method-event"
              "around:postamble:method-event")
        "before/after/around pub-sub order correct"
        :test #'equalp)))
