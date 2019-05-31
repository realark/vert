(in-package :recurse.vert)

;; 3d grid. Upper left = (0,0,0). Towards screen = +z. Left of corner = +x. Down of corner = +y.
;; Standard Distance = 1 Unit
;; Standard Time = 1 Millisecond

(defun units-per-second (x)
  (coerce (/ x 1000) 'single-float))

(defparameter *max-world-unit*
  (coerce (expt 2 30) 'single-float)
  "Max x/y/z value of the world")
(defparameter *min-world-unit*
  (- *max-world-unit*)
  "Min x/y/z value of the world")

(defparameter %uninitialized-interpolated-value% (- *min-world-unit* 1)
  "Performance hack to mark an object a uninitialized. Objects will rarely be on the far negative boundary so this will likely never be a problem.")

(defparameter *max-world-dimension* *max-world-unit*
  "Max width/height of an object face.")
(defparameter *min-world-dimension* #.(expt 10.0 -5)
              "Min width/height of an object face.")

(defparameter *max-camera-scale* 100.0
  ;; 1 unit == 1 pixel
  "Largest zoom*ppu a camera can have")
(defparameter *min-camera-scale* (coerce 1/100 'single-float)
  ;; 512 units = 1 pixel
  "Smallest zoom*ppu a camera can have")

(defparameter *max-vector-magnitude* *max-world-dimension*
  "Largest magnitude a single dimension of an acceleration or velocity vector will have.")
(defparameter *min-vector-magnitude* 0.0
  "Smallest magnitude a single dimension of an acceleration or velocity vector will have.")

(defparameter *movement-threshold* (units-per-second 100)
  "Movable objects with movement below this threshold are not considered moving.")
(defparameter *collision-precision* #.(expt 10.0 -2)
              "Objects will not overlap at or above this number.")

(deftype world-position ()
  "Legal x/y/z world locations"
  `(single-float ,*min-world-unit* ,*max-world-unit*))

(deftype world-dimension ()
  `(single-float ,*min-world-dimension* ,*max-world-dimension*))

(deftype screen-unit ()
  "Legal screen positions and dimensions"
  ;; limit imposed by sdl2 structs
  `(signed-byte 32))

(deftype camera-scale ()
  `(single-float ,*min-camera-scale* ,*max-camera-scale*))

(deftype rotation-degrees ()
  "Clockwise rotation in degrees"
  '(single-float 0f0 360f0))

(deftype rotation-radians()
  "Clockwise rotation in radians"
  `(single-float 0f0 ,tau))

(deftype vector-dimension ()
  "A single dimension of a vector (e.g. velocity and acceleration vectors)."
  `(single-float ,(- *max-vector-magnitude*) ,*max-vector-magnitude*))

(deftype timestamp-ms ()
  "Time in ms since some arbitrary point"
  'fixnum)

(defun assert-units ()
  "Ensure all units fix the expected constraints of the runtime architecture."
  (assert (<= *max-world-unit* (/ (coerce most-positive-fixnum 'single-float) 2)))
  (assert (>= *min-world-unit* (/ (coerce most-negative-fixnum 'single-float) 2)))
  (assert (> *min-world-dimension* (* (expt 2.0 10) least-positive-single-float))))

(defclass physics-context-2d ()
  ((friction-x :initarg :friction-x
               :initform 0.99
               :type (float (0 1))
               :reader friction-x
               :documentation "Friction to apply to x motion. Ranges from 0 (max friction) to 1 (no friction)")
   (drag-y :initarg :drag-y
           :initform 1.0
           :type (float (0 1))
           :reader drag-y
           :documentation "Drag to apply to y motion. Ranges from 0 (max drag) to 1 (no drag)")
   (max-velocity-x :initarg :max-velocity-x
                   :initform (coerce (units-per-second 500) 'single-float)
                   :reader max-velocity-x
                   :documentation "Max x velocity objects can move through this world.")
   (max-velocity-y :initarg :max-velocity-y
                   :initform (units-per-second 2000.0)
                   :reader max-velocity-y
                   :documentation "Max y velocity objects can move through this world.")
   (movement-threshold :initarg :movement-threshold
                       :initform (coerce (units-per-second 100) 'single-float)
                       :reader movement-threshold
                       :documentation "Movable objects with movement below this threshold are not considered moving.")
   (spatial-partition :initarg :spatial-partition
                      :initform (error ":spatial-partition must be specified")
                      :reader spatial-partition
                      :type spatial-partition
                      :documentation "Tracks all objects in this physics context."))
  (:documentation "Relevant physics context used to update game-objects."))
