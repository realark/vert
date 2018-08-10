(in-package :cl-user)
(require 'sb-concurrency)

(defpackage :recurse.vert
  (:use :cl :bordeaux-threads)
  (:import-from :cl-annot.class :export-structure :export-slots :export-class)
  (:export
   ;; event system
   :event-publisher
   :add-subscriber
   :remove-subscriber
   :defevent
   :fire-event

   ;; memory management
   :cache
   :getcache
   :getcache-default
   :clear-cache
   :cache-count
   :remcache
   :evict-oldest-cache
   :counting-cache
   :register-cache
   :deregister-cache
   :get-registered-cache

   ;; core game-object
   :game-object
   :pin-to
   :unpin-from
   :update
   :update-user
   :update-motion
   :render
   :load-resources
   :release-resources
   :recycle

   ;; resource location
   :resource-path
   :test-resource-path
   :use-cached-resource
   :stop-using-cached-resource

   ;; game launcher
   :main
   :quit
   :*engine-manager*
   :game-name
   :audo-player
   :input-manager
   :register-input-device
   :get-input-devices
   :*all-input-id*
   :memory-manager
   :active-scene
   :change-scene
   :clear-color
   :application-window
   :window-size-pixels
   :resize-window
   :toggle-fullscreen
   :ticks

   ;; physics
   :physics-context-2d
   :spatial-partition
   :do-spatial-partition
   :do-neighbors
   :quadtree
   :start-tracking
   :stop-tracking
   ;; position
   :same-sinage
   :point
   :make-point
   :copy-point
   :point-x
   :point-y
   :point-z
   :x
   :y
   :z
   :rotation
   ;; shape and size
   :aabb
   :width
   :height
   :local-points
   :world-points
   :convex-polygon
   ;; collision
   :defcollision
   :collision
   :collidep
   :with-collision-check
   :linear-resolution
   ;; motion
   :kinematic-object
   :moving-towards
   :apply-vector
   :object-moved
   :event-callback-object-moved
   :vector2
   :make-vector2
   :copy-vector2
   :compute-magnitude
   :scale-vector2
   :make-normalized-vector2
   :vector-x
   :vector-y
   :velocity-x
   :velocity-y
   :acceleration-x
   :acceleration-y
   ;;physics utils
   :make-acceleration-vector-seconds
   :make-velocity-vector-seconds

   ;; graphics
   :make-color :r :g :b :a
   :lerp
   :color-mod
   :drawable
   :static-sprite
   :path-to-image
   :camera
   :zoom
   :target

   ;;;; animation
   :animated-drawable
   :time-delay-animation
   :get-new-animation
   :active-effect
   :activate-effect
   :animation
   :animation-effects

   ;; music
   :audio-player
   :play-music
   :stop-music
   :play-sound-effect
   :music-state
   :audio-output-info
   :music-advance
   :event-callback-music-advance
   :sample-process
   :event-callback-sample-process

   ;; ai
   :agent

   ;; input device
   :input-device
   :device-id
   :activate-input
   :deactivate-input
   :get-active-inputs
   :get-deactivated-inputs
   ;; input handler
   :input-handler
   :active-input-device
   :set-default-input-command-map
   :override-input-command-map
   :set-default-command-action-map
   :override-command-action-map

   ;; base scenes
   :scene
   :pause-scene
   :step-scene
   ;; menus
   :menu
   :pause-menu
   :create-menu
   ;; game-scene
   :scene-background
   :game-scene
   :scene-ticks
   :killed
   :event-callback-killed
   :add-to-scene
   :remove-from-scene
   :scene-background

   ;; physics
   :obb
   :obb-outline
   :distance-between
   :project-onto-axis

   ;; utility object
   :composed-object
   :sub-objects
   :touch-tracker
   :touch-region
   :add-touch-region
   :objects-touching
   :obb-touch-tracker

   ;; basic ai plugin
   :back-and-forth
   :feet-touching
   :is-falling
   :direction-tracker
   :facing
   :is-facing
   :push-direction
   :agent-acceleration

   ;; platformer plugin
   :platformer-game-scene
   :gravity-vector
   :apply-gravity
   :jump-start-ts
   :jumper
   :jump
   :is-touching-ground
   :is-jumping
   :on-jump
   :max-jumps))
