(in-package :cl-user)
(require :split-sequence) ; used to parse heap string from (room)

(defpackage :recurse.vert/test
  (:use :cl :recurse.vert)
  (:export
   ;; utils
   :float=
   :point=
   :move
   ;; test introspector
   :test-introspector
   :notice-method-invoked
   :event-callback-name
   :method-invoke-count
   :method-invoke-id
   :method-invoke-order
   ;; test event pub/sub
   :test-event-publisher
   :test-event-subscriber
   :last-arg
   :simple-event
   :method-event
   ;; test objects
   :test-object
   :test-outline
   :num-collisions
   :audio-callback-data
   :test-music-queuer
   :test-circle
   :test-scene
   :start-framerate-measuring
   :stop-framerate-measuring
   :average-framerate
   :min-framerate
   :make-test-scene
   :run-performance-test
   ;; prove test runner
   :run-prove-tests)
  (:documentation "Game implementation used for testing."))

(defpackage :recurse.vert/unit-test
  (:use :cl :recurse.vert :recurse.vert/test :prove))

(defpackage :recurse.vert/integration-test
  (:use :cl :recurse.vert :recurse.vert/test :prove))

(defpackage :recurse.vert/performance-test
  (:use :cl :recurse.vert :recurse.vert/test :prove))
