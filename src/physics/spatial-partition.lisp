;;;; Generic methods for spatial partitioning. Quadtrees, grids, etc.
(in-package :recurse.vert)

(defclass spatial-partition (obb)
  ()
  (:documentation "A spatial partition organizes and tracks a set of world objects."))

(defgeneric start-tracking (spatial-partition game-object)
  (:documentation "Begin tracking GAME-OBJECT in SPATIAL-PARTITION"))

(defgeneric stop-tracking (spatial-partition game-object)
  (:documentation "Stop tracking GAME-OBJECT in SPATIAL-PARTITION"))

(defgeneric find-spatial-partition (object spatial-partition)
  (:documentation "Find and return OBJECT if it is present in SPATIAL-PARTITION"))

(defparameter %dead-object% (make-instance 'game-object)
  "Placeholder for an object which has been removed from the partition
but cannot be removed from the implementation due to iteration.")

(defgeneric %map-partition (function partition)
  (:documentation "Run FUNCTION over every objected in PARTITION.
FUNCTION should be a one-arg fn which takes a partition object"))

(defgeneric %map-neighbors (function game-object partition &optional radius)
  (:documentation "Run FUNCTION over all of GAME-OBJECT's neighbors (determined by PARTITION)
within the RADIUS."))

(defmacro do-spatial-partition ((game-object-name spatial-partition) &body body)
  "Run BODY once over every element (bound to GAME-OBJECT-NAME) in SPATIAL-PARTION."
  (assert (symbolp game-object-name))
  (alexandria:once-only (spatial-partition)
    `(with-slots (is-iterating) ,spatial-partition
       (%map-partition (lambda (,game-object-name)
                         ,@body)
                       ,spatial-partition))))

(defmacro do-neighbors ((game-object spatial-partition neighbor-binding &optional (radius 0.0))
                        &body body)
  (assert (symbolp neighbor-binding))
  (alexandria:once-only (game-object spatial-partition radius)
    `(%map-neighbors (lambda (,neighbor-binding)
                       (declare (optimize (speed 3)))
                       ,@body
                       (values))
                     ,game-object
                     ,spatial-partition
                     ,radius)))
