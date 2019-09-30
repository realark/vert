(in-package :recurse.vert)

;;;; Quadtree spatial partition for tracking objects on the screen.
(defclass layered-quadtree (spatial-partition)
  ((width :initarg :width)
   (height :initarg :height)
   (max-objects :initarg :max-objects
                :initform 20
                :reader max-objects
                :documentation "Max objects to be added before a split.")
   (max-depth :initarg :max-depth
              :initform 5
              :reader max-depth
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
             (when (= (the single-float (z quadtree))
                      (the single-float z-layer))
               (return quadtree)))
        (let ((new-tree (make-instance 'quadtree
                                       :3d-partition layered-quadtree
                                       :z z-layer
                                       :width width
                                       :height height
                                       :max-objects max-objects
                                       :max-depth max-depth)))
          (vector-push-extend new-tree quadtrees)
          (flet ((swap (vec i j)
                   (declare ((vector quadtree) vec)
                            ((integer 0 #.most-positive-fixnum) i j))
                   (rotatef (aref vec i) (aref vec j))))
            (loop with j = 0
               for i downfrom (1- (length quadtrees)) to 1 do
                 (locally (declare ((integer 0 #.most-positive-fixnum) i j))
                   (setf j (1- i))
                   (if (< (z new-tree)
                          (z (elt quadtrees j)))
                       (swap quadtrees i j)
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
