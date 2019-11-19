(in-package :recurse.vert)

;;;; Quadtree spatial partition for tracking objects on the screen.
(defclass layered-quadtree (spatial-partition)
  ((width :initarg :width)
   (height :initarg :height)
   (max-objects :initarg :max-objects
                :initform 100
                :documentation "Max objects to be added before a split.")
   (max-depth :initarg :max-depth
              :initform 7
              :documentation "Maximum number of sub quadtrees allowed.")
   (quadtrees :initform (make-array 5 :adjustable T :fill-pointer 0)
              :documentation "array of quadtrees sorted by z-axis (smallest first)"))
  (:documentation "A collection of quadtrees with 1 quadtree per unique z-layer."))

(defun %get-quadtree-at-layer (layered-quadtree z-layer)
  (declare (optimize (speed 3))
           (world-position z-layer))
  (with-slots (quadtrees width height max-objects max-depth)
      layered-quadtree
    (declare ((vector quadtree) quadtrees))
    (or (loop :for quadtree :across quadtrees :do
             (when (float= (the single-float (z quadtree))
                           (the single-float z-layer))
               (return quadtree)))
        (let ((new-tree (make-instance 'quadtree
                                       :3d-partition layered-quadtree
                                       :z z-layer
                                       :width width
                                       :height height
                                       :max-objects max-objects
                                       :max-depth max-depth)))
          (log:trace "Made new quadtree z layer: ~A : ~A" layered-quadtree (z new-tree))
          (vector-push-extend new-tree quadtrees)
          (loop :for i :downfrom (- (length quadtrees) 1) :to 1 :do
               (locally (declare (fixnum i))
                 (let ((j (1- i)))
                   (declare (fixnum j))
                   (if (< (the single-float (z (elt quadtrees i)))
                          (the single-float (z (elt quadtrees j))))
                       (rotatef (aref quadtrees i) (aref quadtrees j))
                       (return)))))
          new-tree))))

(defmethod start-tracking ((layers layered-quadtree) game-object)
  (start-tracking (%get-quadtree-at-layer
                   layers
                   (z game-object))
                  game-object))

(defmethod stop-tracking ((layers layered-quadtree) game-object)
  (stop-tracking (%get-quadtree-at-layer
                  layers
                  (z game-object))
                 game-object))

(defmethod find-spatial-partition (game-object (layers layered-quadtree))
  (find-spatial-partition game-object
                          (%get-quadtree-at-layer layers (z game-object))))
