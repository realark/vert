;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:vert-asd
  (:use :cl :asdf))

(in-package :vert-asd)

(defsystem vert
  :name "vert"
  :version "0.1"
  :author "Ark"
  :components ((:file "packages")
               (:module core
                        :pathname "src/core"
                        :serial T
                        :components
                        ((:file "utils")
                         (:file "config")
                         (:file "resource")
                         (:file "clos-events")
                         (:file "game-object")))
               (:module memory
                        :depends-on (:core)
                        :pathname "src/memory"
                        :serial T
                        :components
                        ((:file "cache")
                         (:file "evict-oldest-cache")
                         (:file "counting-cache")
                         (:file "resource-cache")))
               (:module physics
                        :depends-on (:core)
                        :pathname "src/physics"
                        :serial T
                        :components
                        (;; basic physics classes
                         (:file "spatial-partition")
                         (:file "physics-context")
                         (:file "point")
                         (:file "vector")
                         (:file "math-utils")
                         ;; shapes and collision
                         (:file "collision")
                         (:file "aabb")
                         (:file "convex-polygon")
                         (:file "obb")
                         ;; motion
                         (:file "motion")
                         (:file "kinematic-object")
                         (:file "linear-motion")
                         (:file "angular-motion")
                         ;; partitioning
                         (:file "quadtree")
                         (:file "layered-quadtree")))
               (:module util-objects
                        :depends-on (:core :physics)
                        :pathname "src/util-objects"
                        :serial T
                        :components ((:file "composed-object")
                                     (:file "touch-tracker")))
               (:module graphics
                        :depends-on (:core :physics)
                        :pathname "src/graphics"
                        :serial T
                        :components
                        ((:file "camera")
                         (:file "color")
                         (:file "gl-utils")
                         (:file "gl-drawable")
                         (:file "gl-sprite")
                         (:file "gl-font")
                         (:file "static-sprite")
                         (:file "animated-sprite")
                         (:file "font-drawable")))
               (:module audio
                        :depends-on (:core)
                        :pathname "src/audio"
                        :serial T
                        :components
                        ((:file "audio-player")
                         (:file "sdl-audio-player")
                         (:file "audio-system")))
               (:module input
                        :depends-on (:core)
                        :pathname "src/input"
                        :serial T
                        :components
                        ((:file "input-device")
                         (:file "input-handler")
                         (:file "input-system")))
               (:module ai
                        :depends-on (:core :physics)
                        :pathname "src/ai"
                        :serial T
                        :components
                        ((:file "agent")
                         (:file "direction-tracker")
                         (:file "back-and-forth")))
               (:module scene
                        :depends-on (:core :physics :graphics :audio :ai :input)
                        :pathname "src/scene"
                        :serial T
                        :components
                        ((:file "scene")
                         (:file "menu")
                         (:file "scene-background")
                         (:file "object-manager")
                         (:file "game-scene")
                         (:file "pause-scene")
                         (:file "pause-menu")))
               (:module state-machine
                :pathname "src/plugins"
                :depends-on (:core)
                :serial T
                :components
                ((:file "state-machine")))
               (:module platformer
                        :pathname "src/plugins/platformer"
                        :depends-on (:scene :state-machine)
                        :serial T
                        :components
                        ((:file "platformer-game-scene")
                         (:file "jumper")))
               (:module tiled
                :pathname "src/plugins/tiled"
                :depends-on (:scene)
                :serial T
                :components
                ((:file "tiled-scene")))
               (:module engine-manager
                        :depends-on (:memory :scene)
                        :pathname "src/engine-manager"
                        :serial T
                        :components
                        ((:file "engine-manager")
                         (:file "application-window")
                         (:file "sdl-application-window")
                         (:file "sdl-engine-manager")
                         (:file "main"))))
  :around-compile "(lambda (compile-fn)
                     (annot:enable-annot-syntax)
                     (funcall compile-fn))"
  :depends-on (#:sdl2
               #:sdl2-mixer
               #:cl-opengl
               #:cl-soil
               #:cl-freetype2
               #:glkit ;; TODO: is this needed?
               #:queues.simple-cqueue
               #:cl-annot
               #:bordeaux-threads
               #:alexandria))

(defsystem vert/test
  :name "vert/test"
  :description "Vert implementation used for testing."
  :pathname "t/"
  :serial t
  :depends-on (:prove :vert)
  :components ((:file "packages")
               (:file "test-impl/test-utils")
               (:file "test-impl/test-introspector")
               (:file "test-impl/test-event-publisher")
               (:file "test-impl/test-event-subscriber")
               (:file "test-impl/test-object")
               (:file "test-impl/test-outline")
               (:file "test-impl/test-circle")
               (:file "test-impl/test-music-queuer")
               (:file "test-impl/test-scene")
               (:file "test-impl/test-runner")))

(defsystem vert/unit-test
  :name "vert/unit-test"
  :pathname "t/unit/"
  :serial t
  :depends-on (:vert/test)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "core/clos-events-test")
               (:file "core/game-object-test")
               (:file "core/composed-object-test")
               (:file "memory/cache-test")
               (:file "physics/physics-utils-test")
               (:file "physics/aabb-test")
               (:file "physics/convex-polygon-test")
               (:file "physics/collision-test")
               (:file "physics/linear-motion-test")
               (:file "physics/touch-tracker-test")
               (:file "physics/spatial-partition-test")
               (:file "graphics/camera-test")
               (:file "graphics/render-interpolation-test")
               (:file "graphics/animation-test")
               (:file "ai/builtin-agent-tests")
               (:file "input/input-handler-test")
               (:file "input/input-device-test")
               (:file "scene/menu-test")
               (:file "scene/pause-scene-test")
               (:file "scene/game-scene-test")
               (:test-file "run-unit-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

(defsystem vert/integration-test
  :name "vert/integration-test"
  :pathname "t/integration/"
  :serial t
  :depends-on (:vert/test)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "services-test")
               (:file "scene-management-test")
               (:test-file "run-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

(defsystem vert/performance-test
  :name "vert/performance-test"
  :pathname "t/performance/"
  :serial t
  :depends-on (:vert/test)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "performance-test")
               (:test-file "run-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
