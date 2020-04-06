;;;; Generic methods for spatial partitioning. Quadtrees, grids, etc.
(in-package :recurse.vert)

(defclass spatial-partition ()
  ()
  (:documentation "A spatial partition organizes and tracks a set of world objects."))

(defgeneric start-tracking (spatial-partition game-object)
  (:documentation "Begin tracking GAME-OBJECT in SPATIAL-PARTITION"))

(defgeneric stop-tracking (spatial-partition game-object)
  (:documentation "Stop tracking GAME-OBJECT in SPATIAL-PARTITION"))

(defgeneric partition-clear (spatial-partition)
  (:documentation "Remove all elements from SPATIAL-PARTITION"))

(defgeneric find-spatial-partition (object spatial-partition)
  (:documentation "Find and return OBJECT if it is present in SPATIAL-PARTITION"))

(defparameter %dead-object% (make-instance 'game-object)
  "Placeholder for an object which has been removed from the partition
but cannot be removed from the implementation due to iteration.")

(defmacro do-quadtree ((game-object-name quadtree &key min-x max-x min-y max-y min-z max-z static-iteration-p skip-static-objects-p) &body body)
  (assert (symbolp game-object-name))
  (alexandria:once-only
   (quadtree min-x max-x min-y max-y min-z max-z static-iteration-p skip-static-objects-p)
   (alexandria:with-gensyms
    (node nodes nodes-fill-pointer iterating-p update-queue update-queue-fill-pointer i j objects objects-fill-pointer)
    `(with-slots ((,nodes nodes)
                  (,iterating-p iterating-p)
                  (,nodes-fill-pointer nodes-fill-pointer))
         ,quadtree
       (declare (optimize (speed 3))
                (sb-ext:muffle-conditions sb-ext:compiler-note))
       (let ((,update-queue (unless ,static-iteration-p
                              (%checkout-update-queue ,quadtree)))
             (,update-queue-fill-pointer 0))
         (unwind-protect
              (progn
                (setf ,iterating-p t)
                (loop :for ,i :from 0 :below ,nodes-fill-pointer :do
                     (let ((,node (elt ,nodes ,i)))
                       (declare (%quadtree-node ,node))
                       (when (or (and (not (null ,min-x))
                                      (not (null ,max-x))
                                      (<= ,min-x
                                          (x ,node)
                                          (+ (x ,node) (width ,node))
                                          ,max-x)
                                      (not (null ,min-y))
                                      (not (null ,max-y))
                                      (<= ,min-y
                                          (y ,node)
                                          (+ (y ,node) (height ,node))
                                          ,max-y))
                                 (%%overlaps-boundary-p ,node ,min-x ,max-x ,min-y ,max-y nil nil))
                         (with-slots ((,objects objects) (,objects-fill-pointer objects-fill-pointer)) ,node
                           (loop :for ,j :from 0 :below ,objects-fill-pointer :do
                              ;; TODO: add optimization to optionally skip static-objects
                                (when (%%overlaps-boundary-p (elt ,objects ,j) ,min-x ,max-x ,min-y ,max-y ,min-z ,max-z)
                                  (if ,static-iteration-p
                                      (let ((,game-object-name (elt ,objects ,j)))
                                        ,@body)
                                      (multiple-value-bind (new-update-queue new-fill-pointer)
                                          (%update-queue-add ,quadtree ,update-queue ,update-queue-fill-pointer (elt ,objects ,j))
                                        (setf ,update-queue new-update-queue
                                              ,update-queue-fill-pointer new-fill-pointer)))))))))
                (setf ,iterating-p nil)
                (unless ,static-iteration-p
                  (loop :for ,i :from 0 :below ,update-queue-fill-pointer :do
                       (when (elt ,update-queue ,i) ; objected may have been nulled out if removed from the partition
                         (let ((,game-object-name (elt ,update-queue ,i)))
                           ,@body)))))
           (when ,iterating-p
             (setf ,iterating-p nil))
           (unless ,static-iteration-p
             (%return-update-queue ,quadtree ,update-queue))
           (values)))))))

(defmacro do-array-backed ((game-object-name array-backed) &body body)
  (assert (symbolp game-object-name))
  (alexandria:once-only (array-backed)
    (alexandria:with-gensyms (obj all-objects)
      `(locally (declare (optimize (speed 3)))
         (with-slots ((,all-objects objects)) ,array-backed
           (declare ((vector (or null game-object)) ,all-objects))
           (loop :for ,obj :across ,all-objects :do
                (let ((,game-object-name ,obj))
                  ,@body)))))))

(defmacro do-spatial-partition ((game-object-name spatial-partition &key min-x max-x min-y max-y min-z max-z static-iteration-p skip-static-objects-p) &body body)
  "Run BODY once over every element (bound to GAME-OBJECT-NAME) in SPATIAL-PARTION. Keyword args are optional optimization hints
When supplied, MIN/MAX boundaries limit iteration to all objects which overlap the boundary.
SKIP-STATIC-OBJECTS-P will skip all objects of type STATIC-OBJECT
STATIC-ITERATION-P optimization hint which tells the partition that no objects will be updated during iteration (useful for object lookup, collision lookup, etc)."
  (assert (symbolp game-object-name))
  (alexandria:once-only (spatial-partition)
    `(cond ((typep ,spatial-partition 'quadtree)
            (do-quadtree (,game-object-name
                          ,spatial-partition
                          :min-x ,min-x
                          :max-x ,max-x
                          :min-y ,min-y
                          :max-y ,max-y
                          :min-z ,min-z
                          :max-z ,max-z
                          :static-iteration-p ,static-iteration-p
                          :skip-static-objects-p ,skip-static-objects-p)
              ,@body))
           ((typep ,spatial-partition 'array-backed)
            (do-array-backed (,game-object-name ,spatial-partition)
              ,@body))
           (t (error "unsupported partition type: ~A" ,spatial-partition)))))


(defmethod find-spatial-partition (object (partition spatial-partition))
  (block find-object
    (do-spatial-partition (obj
                           partition
                           :static-iteration-p t
                           :min-x (x object)
                           :max-x (+ (x object) (width object))
                           :min-y (y object)
                           :max-x (+ (y object) (height object))
                           :min-z (z object)
                           :max-z (z object))
      (when (eq object obj)
        (return-from find-object obj)))))
