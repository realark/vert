(in-package :recurse.vert)

;; work-in-progress rewrite of quadtree

(defconstant %max-quadtree-nodes (expt 2 32))
(defconstant %max-quadtree-objects (expt 2 32))

(defclass quadtree (spatial-partition)
  ((max-objects :initarg :max-objects
                :initform 200
                :documentation "Max objects to be added before a split.")
   (max-depth :initarg :max-depth
              :initform 10
              :documentation "Maximum number of sub quadtrees allowed (root node == depth 0).")
   (min-node-size :initarg :min-node-size
                  :initform (vector2 200.0 200.0))
   (iterating-p :initform nil
                :documentation "Internal consistency checker.
When the quadtree is iterating no operations should be performed on the nodes or objects within the nodes.")
   ;; TODO: profile and find good defaults for depth and object count
   (initial-size :initarg :initial-size
                 :initform (vector2 2000.0 2000.0))
   (initial-position :initarg :initial-position
                     :initform (vector2 -100.0 -100.0))
   (nodes :initform (make-array 10
                                :element-type '(or null %quadtree-node)
                                :initial-element nil)
          :documentation "A simple array of quadtree nodes sorted by x,y ascending")
   (nodes-fill-pointer :initform 0)
   (update-queues :initform (make-array 0
                                        :fill-pointer 0
                                        :adjustable t
                                        :element-type '(simple-array (or null game-object)))
                  :documentation "When a quadtree is iterated, items to be updated will be added to this queue, and upon iteration completion each item in the queue will be updated. Not that `update` is being used loosely, the action run is arbitrary.")
   (update-queues-fill-pointer :initform 0))
  (:documentation "A 2d quadtree"))

(defmethod initialize-instance :after ((quadtree quadtree) &rest args)
  (declare (ignore args))
  (with-slots (nodes nodes-fill-pointer initial-size initial-position) quadtree
    (setf (elt nodes nodes-fill-pointer)
          (make-instance '%quadtree-node
                         :quadtree quadtree
                         :x (x initial-position)
                         :y (y initial-position)
                         :width (width initial-size)
                         :height (height initial-size)))
    (incf nodes-fill-pointer)))

;;; quadtree nodes
(defclass %quadtree-node (obb)
  ((quadtree :initarg :quadtree
             :initform (error ":quadtree required"))
   (depth :initarg :depth :initform 0)
   (objects :initform (make-array 10
                                  :element-type '(or game-object null)
                                  :initial-element nil)
            :documentation "A simple-array of game objects in this node. Static Objects will always be before dynamic objects.")
   (objects-fill-pointer :initform 0)
   (objects-first-dynamic-index :initform 0
                                :documentation "Index of the first dynamic object. This will equal the OBJECTS-FILL-POINTER slot if this node has no dynamic objects."))
  (:documentation "A node in a quadtree"))

(defun %quadtree-create-new-node-for-object (quadtree object)
  "Create a new node which could contain OBJECT. The new node's dimension and position will be based on QUADTREE's dimensions and fit evenly in with the other nodes."
  (declare (game-object object)
           (quadtree quadtree))
  (with-slots (initial-size initial-position) quadtree
    (declare (vector2 initial-size initial-position))
    (multiple-value-bind (object-x object-y z w h)
        (world-dimensions object)
      (declare (ignore z w h)
               (single-float object-x object-y))
      (let* ((new-node-x (+ (* (width initial-size)
                               (floor (/ (- object-x (x initial-position))
                                         (width initial-size))))
                            (x initial-position)))
             (new-node-y (+ (* (height initial-size)
                               (floor (/ (- object-y (y initial-position))
                                         (height initial-size))))
                            (y initial-position)))
             (new-node (make-instance '%quadtree-node
                       :quadtree quadtree
                       :x new-node-x
                       :y new-node-y
                       :width (width initial-size)
                       :height (height initial-size))))
        (%quadtree-add-new-node quadtree new-node)
        new-node))))

(defun %quadtree-add-new-node (quadtree node)
  (with-slots (nodes nodes-fill-pointer iterating-p) quadtree
    (declare ((simple-array (or null %quadtree-node)) nodes)
             ((integer 0 #.%max-quadtree-nodes) nodes-fill-pointer))
    (when iterating-p
      (error "Cannot mutate nodes while iterating"))

    (when (<= (length nodes) nodes-fill-pointer)
      (setf nodes (simple-array-double-size nodes)))
    (setf (elt nodes nodes-fill-pointer)
          node)
    (incf nodes-fill-pointer)
    ;; insert sort the new node
    ;; this shouldn't happen very often
    (loop :for i :from (- nodes-fill-pointer 1) :downto 1 :do
         (let ((other-node (elt nodes (- i 1))))
           ;; first, sort along x axis, ascending.
           (cond ((float= (x node) (x other-node))
                  (cond ((float= (y node) (y other-node))
                         ;; second, sort along y axis, ascending
                         (error "two nodes in quadtree in the same position: ~A -- ~A -- ~A)"
                                quadtree
                                node
                                other-node))
                        ((< (y node) (y other-node))
                         (rotatef (elt nodes i)
                                  (elt nodes (- i 1))))
                        (t ; sorted along x and y axis, stop
                         (return))))
                 ((< (x node) (x other-node))
                  (rotatef (elt nodes i)
                           (elt nodes (- i 1))))
                 (t ; sorted along x and y axis, stop
                  (return)))))))

(defun %%quadtree-node-object-overlaps-node-p (node object)
  (declare (optimize (speed 3))
           (%quadtree-node node)
           (game-object object))
  (multiple-value-bind (object-x object-y z object-w object-h)
      (world-dimensions object)
    (declare (ignore z)
             (single-float object-x object-y object-w object-h))
    (let ((object-max-x (the single-float (+ object-x object-w)))
          (object-max-y (the single-float (+ object-y object-h))))
      (with-accessors ((node-x x) (node-y y)
                       (node-w width) (node-h height))
          node
        (declare (single-float node-x node-y node-w node-h))
        (let ((node-max-x (the single-float (+ node-x node-w)))
              (node-max-y (the single-float (+ node-y node-h))))
          (declare (single-float node-max-x node-max-y))

          (and (or (<= node-x object-x node-max-x)
                   (<= node-x object-max-x node-max-x))
               (or (<= node-y object-y node-max-y)
                   (<= node-y object-max-y node-max-y))))))))

(defun %%quadtree-node-has-object-p (node object)
  (%%quadtree-node-index-for-object node object))

(defun %%quadtree-node-index-for-object (node object)
  (with-slots (objects objects-fill-pointer) node
    (loop :for i :from 0 :below objects-fill-pointer :do
         (when (eq object (elt objects i))
           (return i)))))

(defun %%quadtree-node-insert-objects-array (node object)
  (with-slots (objects objects-fill-pointer objects-first-dynamic-index) node
    (when (<= (length objects) objects-fill-pointer)
      (setf objects (simple-array-double-size objects)))
    (if (typep object 'static-object)
        (progn
          (simple-array-right-shift objects objects-first-dynamic-index objects-fill-pointer)
          (setf (elt objects objects-first-dynamic-index) object)
          (incf objects-first-dynamic-index)
          (incf objects-fill-pointer))
        (progn
          (setf (elt objects objects-fill-pointer) object)
          (incf objects-fill-pointer)))
    (values)))

(defun %%quadtree-node-remove-objects-array (node object)
  (with-slots (objects objects-fill-pointer objects-first-dynamic-index) node
    (let ((obj-index (%%quadtree-node-index-for-object node object)))
      (setf (elt objects obj-index) nil)
      (simple-array-left-shift objects obj-index (- objects-fill-pointer 1))
      (decf objects-fill-pointer)
      (when (< obj-index objects-first-dynamic-index)
        (decf objects-first-dynamic-index))))
  (values))

(defun %quadtree-map-nodes-for-object (function quadtree object)
  "Run FUNCTION on QUADTREE's nodes which overlap with OBJECT. If no nodes overlap, a new node will be created and added to QUADTREE, then have FUNCTION run on it.
OBJECT may or may not be present in the quadtree."

  (declare (optimize (speed 3))
           (game-object object)
           ((function (%quadtree-node)) function)
           (quadtree quadtree))
    (with-slots (nodes nodes-fill-pointer) quadtree
      (declare ((simple-array (or null %quadtree-node)) nodes)
               ((integer 0 #.%max-quadtree-nodes) nodes-fill-pointer))
      (loop :with found-node-for-object-p = nil
         :for i :from 0 :below nodes-fill-pointer :do
           (let ((node (elt nodes i)))
             (when (not (null (parent node)))
               (log:error "quadtree node is expected to have a null transform parent: ~A -- ~A"
                          node
                          (parent node)))
             (when (%%quadtree-node-object-overlaps-node-p node object)
               (setf found-node-for-object-p t)
               (funcall function node)))
         :finally
           (unless found-node-for-object-p
             (let ((node (%quadtree-create-new-node-for-object quadtree object)))
               (funcall function node))))))

(defun %quadtree-node-add-object (node object)
  (declare (optimize (speed 3))
           (%quadtree-node node)
           (game-object object))
  (unless (%%quadtree-node-has-object-p node object)
    (when (slot-value (slot-value node 'quadtree) 'iterating-p)
      (error "Cannot mutate nodes while iterating"))
    (labels ((split-node-p (node)
               "t if the node should be split before adding another object"
               (with-slots (depth objects-fill-pointer quadtree) node
                 (with-slots (max-depth max-objects min-node-size) quadtree
                   (declare (fixnum depth max-depth objects-fill-pointer max-objects))
                   (and (>= objects-fill-pointer max-objects)
                        (< (+ depth 1) max-depth)
                        (>= (/ (width node) 2.0) (width min-node-size))
                        (>= (/ (height node) 2.0) (height min-node-size))))))
             (split-node (node)
               "split NODE into four sub-nodes"
               (log:debug "qtree ndoe split: ~A" node)
               (with-slots (depth objects-fill-pointer objects quadtree) node
                 (with-slots (nodes nodes-fill-pointer) quadtree
                   (let* ((child-width (/ (width node) 2.0))
                          (child-height (/ (height node) 2.0))
                          (east-boundary (+ (x node) child-width))
                          (south-boundary (+ (y node) child-height))
                          (north-west (make-instance '%quadtree-node
                                                     :quadtree quadtree
                                                     :depth (+ depth 1)
                                                     :x (x node)
                                                     :y (y node)
                                                     :width child-width
                                                     :height child-height))
                          (south-west (make-instance '%quadtree-node
                                                     :quadtree quadtree
                                                     :depth (+ depth 1)
                                                     :x (x node)
                                                     :y south-boundary
                                                     :width child-width
                                                     :height child-height))
                          (north-east (make-instance '%quadtree-node
                                                     :quadtree quadtree
                                                     :depth (+ depth 1)
                                                     :x east-boundary
                                                     :y (y node)
                                                     :width child-width
                                                     :height child-height))
                          (south-east (make-instance '%quadtree-node
                                                     :quadtree quadtree
                                                     :depth (+ depth 1)
                                                     :x east-boundary
                                                     :y south-boundary
                                                     :width child-width
                                                     :height child-height)))
                     (loop :for node-index :from 0 :below nodes-fill-pointer :do
                          (when (eq (elt nodes node-index) node)
                            (setf (elt nodes node-index) north-west)
                            (%quadtree-add-new-node quadtree south-west)
                            (%quadtree-add-new-node quadtree north-east)
                            (%quadtree-add-new-node quadtree south-east)
                            (loop :for i :from 0 :below objects-fill-pointer :do
                               ;; instead of using the remove fn just unsubscribe
                               ;; and skip adjusting the array (since it's about to be dereferenced anyways)
                               ;; (%quadtree-node-remove-object node (elt objects i))
                                 (remove-subscriber (elt objects i) node object-moved)
                                 (start-tracking quadtree (elt objects i)))
                            (return))
                        :finally
                          (error "invalid state: node ~A is not in its quadtree ~A" node quadtree)))
                   (values)))))
      (if (split-node-p node)
          (progn
            (split-node node)
            (start-tracking (slot-value node 'quadtree) object))
          (progn
            (log:debug "Adding object ~A to node ~A" object node)
            ;; (break "Adding object ~A to node ~A" object node)
            (%%quadtree-node-insert-objects-array node object)
            (add-subscriber object node object-moved))))))

(defun %quadtree-node-remove-object (node object)
  (declare (optimize (speed 3))
           (%quadtree-node node)
           (game-object object))
  (when (%%quadtree-node-has-object-p node object)
    (when (slot-value (slot-value node 'quadtree) 'iterating-p)
      (error "Cannot mutate nodes while iterating"))
    (log:debug "Removing object ~A from node ~A" object node)
    (remove-subscriber object node object-moved)
    (%%quadtree-node-remove-objects-array node object)
    (values)))

;;; update queue utils
(defun %checkout-update-queue (quadtree)
  (with-slots (update-queues update-queues-fill-pointer) quadtree
    (when (= (length update-queues) update-queues-fill-pointer)
      (vector-push-extend
       (make-array 10
                   :element-type '(or null game-object)
                   :initial-element nil)
       update-queues))
    (let ((queue (elt update-queues update-queues-fill-pointer)))
      (log:trace "checkout update queue: " update-queues-fill-pointer)
      (incf update-queues-fill-pointer)
      queue)))

(defun %return-update-queue (quadtree update-queue)
  (with-slots (update-queues update-queues-fill-pointer) quadtree
    (decf update-queues-fill-pointer)
    (log:trace "return update queue: " update-queues-fill-pointer)
    (unless (eq update-queue (elt update-queues update-queues-fill-pointer))
      (error "unexpected queue return: got ~A expected ~A"
             update-queue
             (elt update-queues update-queues-fill-pointer))))
  (values))

(defun %update-queue-add (quadtree update-queue fill-pointer object)
  (declare (optimize (speed 3))
           ((simple-array (or null game-object)) update-queue)
           (fixnum fill-pointer)
           (game-object object))
  (when (<= (length update-queue) fill-pointer)
    (setf update-queue (simple-array-double-size update-queue))
    (with-slots (update-queues update-queues-fill-pointer) quadtree
      (declare (vector update-queues))
      (setf (elt update-queues (- update-queues-fill-pointer 1)) update-queue)))
  (loop :for i :from 0 :below fill-pointer :do
       (when (eq object (elt update-queue i))
         ;; already in queue
         (return))
     :finally
       (setf (elt update-queue fill-pointer) object)
       (incf fill-pointer))
  ;; return the update queue so callers can re-bind it if we changed the size or added object
  (values update-queue fill-pointer))

;;; implement spatial-partition

(defevent-callback object-moved ((object game-object) (node %quadtree-node))
  (unless (%%inside-boundary-p object
                               (x node) (+ (x node) (width node))
                               (y node) (+ (y node) (height node))
                               nil nil)
    (unless (%%overlaps-boundary-p object
                                   (x node) (+ (x node) (width node))
                                   (y node) (+ (y node) (height node))
                                   nil nil)
      (%quadtree-node-remove-object node object))
    ;; (break "obj between nodes ~A ~A" object node)
    ;; object may need to be added to other nodes
    ;; (break "boundary ~A -- ~A" object node)
    (start-tracking (slot-value node 'quadtree) object)))

;; implement spatial partition methods
(defmethod start-tracking ((quadtree quadtree) (object game-object))
  (%quadtree-map-nodes-for-object
   (lambda (node)
     (%quadtree-node-add-object node object))
   quadtree
   object))

(defmethod stop-tracking ((quadtree quadtree) (object game-object))
  (%quadtree-map-nodes-for-object
   (lambda (node)
     (%quadtree-node-remove-object node object))
   quadtree
   object)
  ;; remove any pending updates on the removed item
  (with-slots (update-queues update-queues-fill-pointer) quadtree
    (loop :for i :from 0 :below (length update-queues) :do
         (let ((queue (elt update-queues i)))
           (loop :for j :from 0 :below (length queue) :do
                (when (eq object (elt queue j))
                  (log:trace "remove pending update for: ~A:~A -> ~A" i j (elt queue j))
                  (setf (elt queue j) nil)))))))

@inline
(defun %%overlaps-boundary-p (object min-x max-x min-y max-y min-z max-z)
  "T if OBJECT overlaps the defined boundary"
  (declare (optimize (speed 3))
           (game-object object))
  (multiple-value-bind (x y z w h)
      (world-dimensions object)
    (declare (single-float x y z w h)
             ((or single-float null) min-x max-x min-y max-y min-z max-z))
    (and (or (null min-x) (>= x min-x) (>= (+ x w) min-x))
         (or (null max-x) (<= x max-x))
         (or (null min-y) (>= y min-y) (>= (+ y h) min-y))
         (or (null max-y) (<= y max-y))
         (or (null min-z) (>= z min-z))
         (or (null max-z) (<= z max-z)))))

@inline
(defun %%inside-boundary-p (object min-x max-x min-y max-y min-z max-z)
  "T if OBJECT fits entirely inside the defined boundary"
  (declare (optimize (speed 3))
           (game-object object))
  (multiple-value-bind (x y z w h)
      (world-dimensions object)
    (declare (single-float x y z w h)
             ((or single-float null) min-x max-x min-y max-y min-z max-z))
    (and (or (null min-x) (and (>= x min-x) (>= (+ x w) min-x)))
         (or (null max-x) (and (<= x max-x) (<= (+ x w) max-x)))
         (or (null min-y) (and (>= y min-y) (>= (+ y h) min-y)))
         (or (null max-y) (and (<= y max-y) (<= (+ y h) max-y)))
         (or (null min-z) (>= z min-z))
         (or (null max-z) (<= z max-z)))))

(defmethod find-spatial-partition (object (quadtree quadtree))
  (block find-object
    (%quadtree-map-nodes-for-object
     (lambda (node)
       (when (%%quadtree-node-has-object-p node object)
         (return-from find-object t)))
     quadtree
     object)
    nil))
