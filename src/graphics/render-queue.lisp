(in-package :recurse.vert)

(defconstant +max-render-queue-size+ (expt 2 32)
  "Performance optimization. The maximum number of objects an render-queue can render.")

@export
(defclass render-queue ()
  ((fill-pointer :initform 0)
   (objects-to-render :initform (make-array 200
                                            :adjustable nil
                                            :element-type 'game-object
                                            :initial-element (make-instance 'game-object))))
  (:documentation "A self-sorting queue of objects to render."))

@export
(defgeneric render-queue-add (render-queue game-object)
  (:documentation "Add GAME-OBJECT to the RENDER-QUEUE.
Object will be rendered the next time RENDER is invoked on the queue.")
  (:method ((queue render-queue) (game-object game-object))
    (declare (optimize (speed 3)))
    (labels ((double-objects-array (current-size current-objects-to-render)
               (log:debug "doubling render-queue size: ~A : ~A -> ~A"
                          queue
                          current-size
                          (* 2 current-size))
               (let ((new-objects-to-render (make-array (min (* current-size 2) +max-render-queue-size+)
                                                        :adjustable nil
                                                        :element-type 'game-object
                                                        :initial-element (make-instance 'game-object))))
                 (declare ((simple-array game-object (*)) current-objects-to-render new-objects-to-render))
                 (loop :for i :from 0 :below current-size :do
                      (setf (elt new-objects-to-render i)
                            (elt current-objects-to-render i)))
                 new-objects-to-render))
             (right-shift-array (array starting-index array-size)
               "Shift all elements to the right, chopping off the final value."
               (loop :for i :from (- array-size 1) :above starting-index :do
                    (setf (elt array i)
                          (elt array (- i 1)))))
             (insert-sorted (game-object objects-to-render array-size)
               (loop :for i :from 0 :below array-size :do
                  ;; note that GAME-OBJECT should already be at the end of the array
                  ;; so we won't do anything if it didn't have a lower priority
                  ;; than the existing elements
                    (when (render< game-object (elt objects-to-render i))
                      (right-shift-array objects-to-render i array-size)
                      (setf (elt objects-to-render i) game-object)
                      (return)))))
      (with-slots (fill-pointer objects-to-render) queue
        (declare ((simple-array game-object (*)) objects-to-render)
                 ((integer 0 #.+max-render-queue-size+) fill-pointer))
        (cond ((= fill-pointer +max-render-queue-size+)
               (error "max render-queue size exceeded: ~A" +max-render-queue-size+))
              ((= fill-pointer (array-total-size objects-to-render))
               (setf objects-to-render (double-objects-array fill-pointer objects-to-render))))
        (setf (elt objects-to-render fill-pointer) game-object)
        (setf fill-pointer (+ 1 fill-pointer))
        (insert-sorted game-object objects-to-render fill-pointer)))
    (values)))

(defgeneric render-queue-remove (render-queue game-object)
  (:documentation "Remove GAME-OBJECT from the queue of objects to render. No effect if GAME-OBJECT is not in the queue.")
  (:method ((queue render-queue) (game-object game-object))
    ;; this may not be the most optimal, but it shouldn't be called on a hot codepath
    (with-slots (objects-to-render) queue
      (setf objects-to-render (delete game-object objects-to-render)))
    (log:trace "QUEUE REMOVE: removed ~A from render queue" game-object)
    (values)))

@export
(defun render-queue-reset (render-queue)
  (declare (render-queue render-queue))
  (setf (slot-value render-queue 'fill-pointer) 0))

(defmethod render ((queue render-queue) update-percent camera gl-context)
  (declare (optimize (speed 3)))
  (with-slots (fill-pointer objects-to-render) queue
    (declare (fixnum fill-pointer)
             ((simple-array game-object (*)) objects-to-render))
    (log:trace "~%Rendering ~A objects" fill-pointer)
    (loop :for i :from 0 :below fill-pointer :do
         (render (elt objects-to-render i) update-percent camera gl-context)
         (when (log:trace)
           (unless (typep (elt objects-to-render i) 'static-object)
             (log:trace "-- ~A" (elt objects-to-render i))))))
  (values))

;;;; render-compare api and utils

(declaim (ftype (function (game-object game-object) (integer -1 1)) render-compare-same-z-layer))
@export
(defgeneric render-compare-same-z-layer (object1 object2)
  (:documentation "Set the render priority for objects on the same z-layer.")
  (:method ((object1 game-object) (object2 game-object))
    ;; no preference by default
    0))

(declaim (ftype (function (game-object game-object) (integer -1 1)) render-compare))
@export
(defgeneric render-compare (object1 object2)
  (:documentation "Determines which object should render after the other. 1 is returned if OBJECT1 has priority, -1 if OBJECT2 has priority, and 0 if both objects have equal priority. Note that objects with a higher priority will be rendered \"on top\" of objects with lower priority.")
  (:method ((object1 game-object) (object2 game-object))
    (declare (optimize (speed 3)))
    (labels ((z-layer (object)
               (cond ((and (not (null (parent object)))
                           (typep object 'obb))
                      (multiple-value-bind (x y z w h) (world-dimensions object)
                        (declare (single-float z)
                                 (ignore x y w h))
                        z))
                     (t (let ((z (z object)))
                          (declare (single-float z))
                          z)))))
      (let ((z1 (z-layer object1))
            (z2 (z-layer object2)))
        (declare (single-float z1 z2))
        (cond ((> z1 z2) 1)
              ((< z1 z2) -1)
              (t (render-compare-same-z-layer object1 object2)))))))

@export
(defun render< (object1 object2)
  (eql -1 (render-compare object1 object2)))

@export
(defun render> (object1 object2)
  (eql 1 (render-compare object1 object2)))

@export
(defun render= (object1 object2)
  (eql 0 (render-compare object1 object2)))

@export
(defun render<= (object1 object2)
  (or (render< object1 object2)
      (render= object1 object2)))

@export
(defun render>= (object1 object2)
  (or (render> object1 object2)
      (render= object1 object2)))
