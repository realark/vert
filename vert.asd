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
                         (:file "vector")
                         (:file "math-utils")
                         (:file "transform")
                         ;; shapes and collision
                         (:file "collision")
                         (:file "obb")
                         (:file "convex-polygon")
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
                        :components ((:file "touch-tracker")))
               (:module graphics
                        :depends-on (:core :physics)
                        :pathname "src/graphics"
                        :serial T
                        :components
                        ((:file "camera")
                         (:file "color")
                         (:file "matrix-interpolator")
                         (:file "gl-utils")
                         (:static-file "polygon-shader.vert")
                         (:static-file "polygon-shader.frag")
                         (:static-file "sprite-shader.vert")
                         (:static-file "sprite-shader.frag")
                         (:static-file "font-shader.vert")
                         (:static-file "font-shader.frag")
                         (:file "draw-component")
                         (:file "drawable")
                         (:file "static-sprite")
                         (:file "animated-sprite")
                         (:file "font-drawable")
                         (:file "render-queue")))
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
                         (:file "direction-tracker")))
               (:module scene
                        :depends-on (:core :physics :graphics :audio :ai :input)
                        :pathname "src/scene"
                        :serial T
                        :components
                        ((:file "scene")
                         (:file "overlay")
                         (:file "menu")
                         (:file "scene-background")
                         (:file "game-scene")
                         (:file "pause-scene")
                         (:file "pause-menu")
                         (:file "object-manager")))
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
                        ((:file "game-stats")
                         (:file "engine-manager")
                         (:file "application-window")
                         (:file "sdl-engine-manager")
                         (:file "main")))
               (:module dialog
                        :pathname "src/plugins"
                        :depends-on (:scene)
                        :serial T
                        :components
                        ((:file "dialog"))))
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
               #:log4cl
               #:bordeaux-threads
               #:alexandria))

(defsystem vert/test
  :name "vert/test"
  :description "Vert test utils"
  :pathname "t/"
  :serial t
  :depends-on (:prove :vert)
  :components ((:file "packages")
               (:file "test-runner")
               (:file "core-test")
               (:file "cache-test")
               (:file "spatial-partition-test")))
