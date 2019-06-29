(in-package :recurse.vert)

(defclass quadtree (spatial-partition)
  ((max-objects :initarg :max-objects
                :initform 10
                :reader max-objects
                :documentation "Max objects to be added before a split.")
   (max-depth :initarg :max-depth
              :initform 5
              :reader max-depth
              :documentation "Maximum number of sub quadtrees allowed.")
   (level :initarg :level :initform 0)
   (parent :initarg :parent :initform nil :reader parent)
   (objects :initform (make-array 10 :fill-pointer 0 :adjustable T)
            :accessor objects
            :documentation "Objects at this node in the tree.")
   (iteration-context
    :initform nil
    :documentation "A stack with each element being a list of objects to skip for the current update.")
   (children :initform nil
             :accessor children
             :documentation "Child quadtrees")
   (3d-partition :initarg :3d-partition
                 :initform nil
                 :documentation "A 3d spatial partition to call into when objects move outside of the quadtree's z-layer."))
  (:documentation "A 2d quadtree"))

(defmethod initialize-instance :after ((quadtree quadtree) &rest args)
  (declare (ignore args))
  (when (eq (%quadtree-root quadtree) quadtree)
    (setf (slot-value quadtree 'iteration-context)
          (make-array 0 :fill-pointer 0 :adjustable T))))

(defun %mark-dead (object node)
  "Replace OBJECT with dead-object"
  (with-slots (objects) node
    (let ((pos (position object objects)))
      (assert (not (null pos)))
      (setf (elt objects pos) %dead-object%))))

(defun %mark-updated (object node)
  "Mark an object as updated for a single level of an iteration."
  (let ((skips (%update-skips node)))
    (unless (find object skips)
      (vector-push-extend object skips))))

(defun %update-skips (node)
  "An array objects for update skipping. Based on the current iteration context."
  (with-slots (iteration-context) (%quadtree-root node)
    (elt iteration-context
         (1- (fill-pointer iteration-context)))))

(defun %push-iteration-context (node)
  "Push a fresh update-skip list on to the iteration stack"
  (when (eq (%quadtree-root node) node)
    (with-slots (iteration-context) (%quadtree-root node)
      (if (= (length iteration-context) (array-total-size iteration-context))
          (vector-push-extend (make-array 5 :fill-pointer 0 :adjustable T)
                              iteration-context)
          (incf (fill-pointer iteration-context))))))

(defun %pop-iteration-context (node)
  "Pop the most recent update skip-list from the iteration stack"
  (when (eq (%quadtree-root node) node)
    (let ((update-skips (%update-skips node)))
      (unless update-skips
        (error "no iteration context to pop"))
      (setf (fill-pointer update-skips) 0))
    (with-slots (iteration-context) (%quadtree-root node)
      (decf (fill-pointer iteration-context)))))

(defun %is-iterating (node)
  (> (length (slot-value (%quadtree-root node) 'iteration-context)) 0))

(defun %quadtree-clear (quadtree)
  (if (%is-iterating quadtree)
      (error "can't clear quadtree during iteration"))
  (loop for object across (objects quadtree) do
       (remove-subscriber object quadtree object-moved-all)
     finally (setf (fill-pointer (objects quadtree)) 0))
  (when (children quadtree)
    (loop for child across (children quadtree) do
         (%quadtree-clear child)
       finally (setf (slot-value quadtree 'children) nil))))

(defun %quadtree-split (tree)
  "Split the node into four children. This should only be called once on the given node."
  (with-slots (children level max-depth max-objects width height 3d-partition) tree
    (unless children
      (with-accessors ((width width) (height height) (x x) (y y) (z z)) tree
        (let* ((child-width (/ width 2))
               (child-height (/ height 2))
               (nw (make-instance 'quadtree
                                  :3d-partition 3d-partition
                                  :level (1+ level)
                                  :parent tree
                                  :max-objects max-objects
                                  :max-depth max-depth
                                  :x x
                                  :y y
                                  :z z
                                  :width child-width
                                  :height child-height))
               (sw (make-instance 'quadtree
                                  :3d-partition 3d-partition
                                  :level (1+ level)
                                  :parent tree
                                  :max-objects max-objects
                                  :max-depth max-depth
                                  :x x
                                  :y (+ y child-height)
                                  :z z
                                  :width child-width
                                  :height child-height))
               (se (make-instance 'quadtree
                                  :3d-partition 3d-partition
                                  :level (1+ level)
                                  :parent tree
                                  :max-objects max-objects
                                  :max-depth max-depth
                                  :x (+ x child-width)
                                  :y (+ y child-height)
                                  :z z
                                  :width child-width
                                  :height child-height))
               (ne (make-instance 'quadtree
                                  :3d-partition 3d-partition
                                  :level (1+ level)
                                  :parent tree
                                  :max-objects max-objects
                                  :max-depth max-depth
                                  :x (+ x child-width)
                                  :y y
                                  :z z
                                  :width child-width
                                  :height child-height)))
          (setf children (vector nw ne sw se)))))))

(proclaim '(inline %quadtree-root))
(defun %quadtree-root (quadtree)
  (declare (optimize (speed 3)))
  (loop with qt = quadtree
     while (slot-value qt 'parent) do
       (setf qt (slot-value qt 'parent))
     finally (return qt)))

(defun %quadtree-double-root-area (quadtree)
  (when (%is-iterating quadtree)
    (error "FIXME: resize during iteration not implemented"))
  (unless (eq quadtree (%quadtree-root quadtree))
    (error "Attempted to resize non-root quadtree node."))
  (let ((all-objects '()))
    (declare (dynamic-extent all-objects))
    (do-spatial-partition (object quadtree)
      (push object all-objects))
    (%quadtree-clear quadtree)
    (with-accessors ((x x) (y y) (w width) (h height)) quadtree
      (decf x (/ w 2))
      (decf y (/ h 2))
      (setf w (* w 2))
      (setf h (* h 2)))
    (loop while all-objects do
         (start-tracking quadtree (pop all-objects)))))

(defun %inside-of (game-object quadtree)
  "T if the object is entirely inside of the quadtree."
  ;; NOTE: quadtree ignores Z axis
  (declare (optimize (speed 3)))
  (with-slots ((w1 width) (h1 height)
               (p1 world-position))
      game-object
    (with-accessors ((x1 x) (y1 y)) p1
      (with-slots ((w2 width) (h2 height)
                   (p2 world-position))
          quadtree
        (with-accessors ((x2 x) (y2 y)) p2
          (declare (world-position x1 y1 x2 y2)
                   (world-dimension w1 h1 w2 h2))
          (and (> x1 x2)
               (< (+ x1 w1) (+ x2 w2))
               (> y1 y2)
               (< (+ y1 h1) (+ y2 h2))))))))

(defun %node-for-object (object quadtree)
  (with-slots (children level) quadtree
    (when (%inside-of object quadtree)
      (or (and children
               (loop with match = nil
                  for child across children do
                    (setf match (%node-for-object object child))
                    (when match (return match))))
          quadtree))))

(defun %rebalance (quadtree)
  (with-slots (max-objects max-depth level objects children)
      quadtree
    (setf objects (delete %dead-object% objects))
    (when (and (null children)
               (> (length objects) max-objects)
               (< level max-depth))
      (let ((objects-tmp objects))
        (%quadtree-split quadtree)
        (setf objects (make-array 10 :fill-pointer 0 :adjustable T))
        (loop for obj-tmp across objects-tmp do
             (remove-subscriber obj-tmp quadtree object-moved-all)
             (start-tracking quadtree obj-tmp))))))

;; expand/rebalance quadtree when tracked objects move
(defevent-callback object-moved-all ((object game-object) (quadtree quadtree))
  (if (= (z object) (z quadtree))
      (unless (%inside-of object quadtree)
        (with-slots (objects) quadtree
          (stop-tracking quadtree object)
          (when (%is-iterating quadtree)
            (%mark-updated object quadtree))
          (start-tracking quadtree object)))
      ;; object has moved outside of the quadtree's z-layer
      (with-slots (objects 3d-partition) quadtree
        (stop-tracking quadtree object)
        (when 3d-partition
          (start-tracking 3d-partition object)))))

;; implement spatial partition methods

(defmethod start-tracking ((quadtree quadtree) (object game-object))
  (let ((node (%node-for-object object quadtree)))
    (unless node
      (if (parent quadtree)
          (return-from start-tracking (start-tracking (parent quadtree) object))
          (progn
            (%quadtree-double-root-area quadtree)
            (return-from start-tracking (start-tracking quadtree object)))))

    (with-slots (objects) node
      (add-subscriber object node object-moved-all)
      (vector-push-extend object objects)
      (unless (%is-iterating node)
        (%rebalance node)))))

(defmethod stop-tracking ((quadtree quadtree) (object game-object))
  (with-slots (children objects) quadtree
    (or (and children
             (loop for child across children do
                  (when (stop-tracking child object)
                    (return object))))
        (when (find object objects)
          (remove-subscriber object quadtree object-moved-all)
          (if (%is-iterating quadtree)
              (%mark-dead object quadtree)
              (setf objects (delete object objects)))
          object))))

(defmethod find-spatial-partition (game-object (quadtree quadtree))
  (let ((node (%node-for-object game-object quadtree)))
    (when node
      (find game-object (objects node)))))

(defmethod %map-partition ((function function) (quadtree quadtree))
  (declare (optimize (speed 3)))
  (with-slots (children objects level) quadtree
    (declare (fixnum level)
             ((vector T) objects))
    (unwind-protect
         (progn
           (%push-iteration-context quadtree)
           (when children
             (loop for child across children do
                  (%map-partition function child)))
           (loop with update-skips = (%update-skips quadtree)
              for object across objects do
                (locally (declare ((vector T) update-skips)
                                  (game-object object))
                  (unless (or (eq object %dead-object%)
                              (find object update-skips))
                    (funcall function object)))))
      (%pop-iteration-context quadtree)
      (unless (%is-iterating quadtree)
        (%rebalance quadtree))
      (values))))

(defmethod %map-neighbors ((function function) (game-object game-object)
                           (quadtree quadtree) &optional (radius 0.0))
  (declare (optimize (speed 3))
           (world-position radius))
  (unless (= 0.0 radius) (error "FIXME: quadtree radius lookup not implemented"))
  (let ((node (%node-for-object game-object quadtree)))
    (unless node
      (error "game-object (~A) not tracked by quadtree (~A)" game-object quadtree))
    (labels ((map-neighbors (node)
               (with-slots (objects) node
                 (declare ((vector T) objects))
                 (loop for neighbor across objects do
                    ;; (format T "check neighbor: ~A~%" neighbor)
                      (unless (or (eq neighbor %dead-object%)
                                  (eq neighbor game-object))
                        (when (collidep game-object neighbor)
                          (funcall function neighbor))))))
             (map-children (children-vector)
               (when children-vector
                 (loop for child across children-vector do
                      (map-neighbors child)
                      (map-children (slot-value child 'children)))))
             (map-parents (node)
               (when node
                 (map-neighbors node)
                 (map-parents (slot-value node 'parent)))
               (values)))
      (map-children (slot-value node 'children))
      (map-neighbors node)
      (map-parents (slot-value node 'parent))
      (values))))
