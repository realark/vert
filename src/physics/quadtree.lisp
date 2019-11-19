(in-package :recurse.vert)

(defclass quadtree (obb spatial-partition)
  ((max-objects :initarg :max-objects
                :initform 10
                :documentation "Max objects to be added before a split.")
   (max-depth :initarg :max-depth
              :initform 5
              :documentation "Maximum number of sub quadtrees allowed.")
   (level :initarg :level :initform 0)
   (quadtree-parent :initarg :quadtree-parent
                    :initform nil
                    :reader quadtree-parent)
   (objects :initform (make-array 10
                                  :fill-pointer 0
                                  :element-type 'game-object
                                  :adjustable T
                                  :initial-element %dead-object%)
            :accessor quadtree-objects
            :documentation "Objects at this node in the tree.")
   (iteration-context
    :initform nil
    :documentation "A stack with each element being a list of objects to skip for the current update.")
   (quadtree-children :initform nil
                      :accessor quadtree-children
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
    (unless (find object skips :test #'eq)
      (vector-push-extend object skips))))

(defun %update-skips (node)
  "An array objects for update skipping. Based on the current iteration context."
  (with-slots (iteration-context) (%quadtree-root node)
    (if (= 0 (fill-pointer iteration-context))
        #()
        (elt iteration-context
             (1- (fill-pointer iteration-context))))))

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
  (loop for object across (quadtree-objects quadtree) do
       (remove-subscriber object quadtree object-moved)
     finally (setf (fill-pointer (quadtree-objects quadtree)) 0))
  (when (quadtree-children quadtree)
    (loop for child across (quadtree-children quadtree) do
         (%quadtree-clear child)
       finally (setf (slot-value quadtree 'quadtree-children) nil))))

(defun %quadtree-split (tree)
  "Split the node into four children. This should only be called once on the given node."
  (with-slots (quadtree-children level max-depth max-objects width height 3d-partition) tree
    (unless quadtree-children
      (with-accessors ((width width) (height height) (x x) (y y) (z z)) tree
        (let* ((child-width (/ width 2))
               (child-height (/ height 2))
               (nw (make-instance 'quadtree
                                  :3d-partition 3d-partition
                                  :level (1+ level)
                                  :quadtree-parent tree
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
                                  :quadtree-parent tree
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
                                  :quadtree-parent tree
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
                                  :quadtree-parent tree
                                  :max-objects max-objects
                                  :max-depth max-depth
                                  :x (+ x child-width)
                                  :y y
                                  :z z
                                  :width child-width
                                  :height child-height)))
          (setf quadtree-children
                (make-array 4
                            :element-type 'quadtree
                            :initial-contents (list nw ne sw se)
                            :adjustable nil)))))))

@inline
(defun %quadtree-root (quadtree)
  (declare (optimize (speed 3)))
  (loop :with qt = quadtree
     :while (slot-value qt 'quadtree-parent) :do
       (setf qt (slot-value qt 'quadtree-parent))
     :finally (return qt)))

(defun %quadtree-double-root-area (quadtree)
  (declare (optimize (speed 3)))
  (when (%is-iterating quadtree)
    (error "FIXME: resize during iteration not implemented"))
  (unless (eq quadtree (%quadtree-root quadtree))
    (error "Attempted to resize non-root quadtree node."))
  (let ((all-objects (list)))
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
  (multiple-value-bind (x1 y1 z1 w1 h1)
      (world-dimensions game-object)
    (declare (ignore z1))
    (with-accessors ((x2 x) (y2 y)
                     (w2 width) (h2 height)
                     (p2 world-position))
        quadtree
      (declare (world-position x1 y1 x2 y2)
               (world-dimension w1 h1 w2 h2))
      (and (> x1 x2)
           (< (+ x1 w1) (+ x2 w2))
           (> y1 y2)
           (< (+ y1 h1) (+ y2 h2))))))

(defun %node-for-object (object quadtree)
  (declare (optimize (speed 3)))
  (with-slots ((children quadtree-children) level) quadtree
    (when (%inside-of object quadtree)
      (or (and children
               (loop :with match = nil
                  :for child :across (the (simple-array quadtree (4)) children) :do
                    (setf match (%node-for-object object child))
                    (when match (return match))))
          quadtree))))

@inline
(defun %rebalance (quadtree)
  (declare (optimize (speed 3)))
  (with-slots (max-objects max-depth level objects (children quadtree-children))
      quadtree
    (declare ((vector game-object) objects)
             (fixnum max-objects level max-depth))
    (setf objects (delete %dead-object% objects :test #'eq))
    (when (and (null children)
               (> (length objects) max-objects)
               (< level max-depth))
      (let ((objects-tmp objects))
        (declare ((vector game-object) objects-tmp))
        (%quadtree-split quadtree)
        (setf objects (make-array 10 :fill-pointer 0 :adjustable T))
        (loop :for obj-tmp :across objects-tmp :do
             (remove-subscriber obj-tmp quadtree object-moved)
             (start-tracking quadtree obj-tmp))))))

;; expand/rebalance quadtree when tracked objects move
(defevent-callback object-moved ((object game-object) (quadtree quadtree))
  (declare (optimize (speed 3)))
  (if (= (the single-float (z object))
         (the single-float (z quadtree)))
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
  (declare (optimize (speed 3)))
  (let ((node (%node-for-object object quadtree)))
    (unless node
      (if (quadtree-parent quadtree)
          (return-from start-tracking (start-tracking (quadtree-parent quadtree) object))
          (progn
            (%quadtree-double-root-area quadtree)
            (return-from start-tracking (start-tracking quadtree object)))))

    (with-slots (objects) node
      (declare ((vector game-object) objects))
      (add-subscriber object node object-moved)
      (vector-push-extend object objects)
      (unless (%is-iterating node)
        (%rebalance node)))))

(defmethod stop-tracking ((quadtree quadtree) (object game-object))
  (declare (optimize (speed 3)))
  (with-slots ((children quadtree-children) objects) quadtree
    (declare ((vector game-object) objects))
    (or (and children
             (loop :for child :across (the (simple-array quadtree (4)) children) :do
                  (when (stop-tracking child object)
                    (return object))))
        (when (find object objects :test #'eq)
          (remove-subscriber object quadtree object-moved)
          (if (%is-iterating quadtree)
              (%mark-dead object quadtree)
              (setf objects (delete object objects :test #'eq)))
          object))))

(defmethod find-spatial-partition (game-object (quadtree quadtree))
  (declare (optimize (speed 3)))
  (let ((node (%node-for-object game-object quadtree)))
    (when node
      (find game-object (the (vector game-object) (quadtree-objects node)) :test #'eq))))

@inline
(defun %in-boundary-p (object min-x max-x min-y max-y min-z max-z)
  (declare (optimize (speed 3))
           (transform object)
           ((or null world-position) min-x max-x min-y max-y min-z max-z))
  (multiple-value-bind (x y z w h) (world-dimensions object)
    (locally (declare (world-position x y z)
                      (world-dimension w h))
      (and (or (null min-x) (>= x min-x) (>= (+ x w) min-x))
           (or (null max-x) (<= x max-x))
           (or (null min-y) (>= y min-y) (>= (+ y h) min-y))
           (or (null max-y) (<= y max-y))
           (or (null min-z) (>= z min-z))
           (or (null max-z) (<= z max-z))))))
