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

(defmacro do-quadtree ((game-object-name quadtree &key min-x max-x min-y max-y min-z max-z) &body body)
  "Optimized implementation of DO-SPATIAL-PARTITION for quadtrees."
  (assert (symbolp game-object-name))
  (alexandria:once-only (quadtree min-x max-x min-y max-y min-z max-z)
    (alexandria:with-gensyms (quadtrees-to-iterate current-quad children child objects level object)
      `(let ((,quadtrees-to-iterate (list ,quadtree)))
         (declare (optimize (speed 3))
                  (dynamic-extent ,quadtrees-to-iterate))
         (loop :while ,quadtrees-to-iterate :do
              (let ((,current-quad (pop ,quadtrees-to-iterate)))
                (with-slots ((,children children) (,objects objects) (,level level)) ,current-quad
                  (declare (fixnum ,level)
                           ((vector game-object) ,objects))
                  (when (%in-boundary-p ,current-quad ,min-x ,max-x ,min-y ,max-y ,min-z ,max-z)
                    (unwind-protect
                         (progn
                           (%push-iteration-context ,current-quad)
                           (when ,children
                             (loop :for ,child :across (the (simple-array quadtree (4)) ,children) :do
                                  (push ,child ,quadtrees-to-iterate)))
                           (loop :with update-skips = (%update-skips ,current-quad)
                              :for ,object :across (the (vector game-object) ,objects) :do
                                (locally (declare ((vector T) update-skips)
                                                  (game-object ,object))
                                  (when (and (not (eq %dead-object% ,object))
                                             (not (find ,object update-skips))
                                             (%in-boundary-p ,object ,min-x ,max-x ,min-y ,max-y ,min-z ,max-z))
                                    (let ((,game-object-name ,object))
                                      ,@body)))))
                      (%pop-iteration-context ,current-quad)
                      (unless (%is-iterating ,current-quad)
                        (%rebalance ,current-quad))
                      (values))))))))))

(defmacro do-layred-quadtree ((game-object-name layered-quadtree &key min-x max-x min-y max-y min-z max-z) &body body)
  (assert (symbolp game-object-name))
  (alexandria:once-only (min-z max-z)
    (alexandria:with-gensyms (quadtree)
      `(loop :for ,quadtree :across (slot-value ,layered-quadtree 'quadtrees) :do
            (when (<= (or ,min-z (z ,quadtree))
                      (z ,quadtree)
                      (or ,max-z (z ,quadtree)))
              (do-quadtree (,game-object-name
                            ,quadtree
                            :min-x ,min-x
                            :max-x ,max-x
                            :min-y ,min-y
                            :max-y ,max-y
                            :min-z ,min-z
                            :max-z ,max-z)
                ,@body))))))

(defmacro do-spatial-partition ((game-object-name spatial-partition &key min-x max-x min-y max-y min-z max-z) &body body)
  "Run BODY once over every element (bound to GAME-OBJECT-NAME) in SPATIAL-PARTION."
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
                          :max-z ,max-z)
              ,@body))
           ((typep ,spatial-partition 'layered-quadtree)
            (do-layred-quadtree (,game-object-name
                                 ,spatial-partition
                                 :min-x ,min-x
                                 :max-x ,max-x
                                 :min-y ,min-y
                                 :max-y ,max-y
                                 :min-z ,min-z
                                 :max-z ,max-z)
              ,@body))
           (t (error "unsupported partition type: ~A" ,spatial-partition)))))
