(in-package :cl-user)

(defpackage :recurse.vert
  (:use #:cl #:bordeaux-threads)
  (:nicknames #:vert)
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

   ;; core game-object
   :game-object
   :update
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
   :quadtree
   :start-tracking
   :stop-tracking
   ;; position
   :same-sinage
   :x
   :y
   :z
   :rotation
   ;; shape and size
   :width
   :height
   :scale-x
   :scale-y
   ;; collision
   :defcollision
   :collision
   :collidep
   :with-collision-check
   :linear-resolution
   ;; motion
   :object-moved
   :event-callback-object-moved
   ;;physics utils
   :make-acceleration-vector-seconds
   :make-velocity-vector-seconds

   ;; graphics
   :make-color :r :g :b :a
   :lerp
   :color
   :drawable
   :static-sprite
   :path-to-sprite
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
   :touch-tracker
   :touch-region
   :add-touch-region
   :objects-touching
   :obb-touch-tracker

   ;; global state
   :*engine-manager*
   :*config*
   :*scene*
   :*timestep*
   :*dev-mode*
   :*audio*))
