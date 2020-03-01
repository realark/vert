(in-package :recurse.vert)

(defconstant +max-render-queue-size+ (expt 2 32)
  "Performance optimization. The maximum number of objects an render-queue can render.")

(defvar %render-dummy%
  (make-instance 'game-object
                 :object-id 'render-dummy) )

@export
(defclass render-queue ()
  ((fill-pointer :initform 0)
   (objects-to-render :initform (make-array 200
                                            :adjustable nil
                                            :element-type 'game-object
                                            :initial-element %render-dummy%)))
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
                    (when (eq game-object (elt objects-to-render i))
                      (return))
                    (when (render< game-object (elt objects-to-render i))
                      (right-shift-array objects-to-render i array-size)
                      (setf (elt objects-to-render i) game-object)
                      (return))))
             (already-in-queue-p (game-object objects-to-render array-size)
               (loop :for i :from 0 :below array-size :do
                    (when (eq game-object (elt objects-to-render i))
                      (return t)))))
      (with-slots (fill-pointer objects-to-render) queue
        (declare ((simple-array game-object (*)) objects-to-render)
                 ((integer 0 #.+max-render-queue-size+) fill-pointer))
        (cond ((= fill-pointer +max-render-queue-size+)
               (error "max render-queue size exceeded: ~A" +max-render-queue-size+))
              ((= fill-pointer (array-total-size objects-to-render))
               (setf objects-to-render (double-objects-array fill-pointer objects-to-render))))
        (unless (already-in-queue-p game-object objects-to-render fill-pointer)
          (setf (elt objects-to-render fill-pointer) game-object)
          (setf fill-pointer (+ 1 fill-pointer))
          (insert-sorted game-object objects-to-render fill-pointer))))
    (values))
  (:method ((queue render-queue) (camera camera))
    ;; no-op
    (values)))

(defgeneric render-queue-remove (render-queue game-object)
  (:documentation "Remove GAME-OBJECT from the queue of objects to render. No effect if GAME-OBJECT is not in the queue.")
  (:method ((queue render-queue) (game-object game-object))
    ;; this may not be the most optimal, but it shouldn't be called on a hot codepath
    (labels ((left-shift-array (array start-index end-index)
               "Shift all elements to the left for elements >= START-INDEX and < END-INDEX"
               (loop :for i :from start-index :below end-index :do
                    (setf (elt array i)
                          (elt array (+ i 1))))))
      (with-slots (objects-to-render fill-pointer) queue
        (loop :for i :from 0 :below fill-pointer :do
             (when (eq (elt objects-to-render i)
                       game-object)
               (left-shift-array objects-to-render i fill-pointer)
               (setf (elt objects-to-render fill-pointer) %render-dummy%)
               (decf fill-pointer)
               (log:trace "QUEUE REMOVE: removed ~A from render queue" game-object)
               (return))
             :finally
             (log:trace "QUEUE REMOVE: object not found for removal: ~A" game-object))))
    (values)))

@export
(defun render-queue-reset (render-queue)
  (declare (render-queue render-queue))
  (setf (slot-value render-queue 'fill-pointer) 0))

(defmethod render ((queue render-queue) update-percent camera gl-context)
  (declare (optimize (speed 3)))
  (with-slots (fill-pointer objects-to-render) queue
    (declare ((integer 0 #.+max-render-queue-size+) fill-pointer)
             ((simple-array game-object (*)) objects-to-render))
    (log:trace "~%Rendering ~A objects" fill-pointer)

    (let* ((render-delta 16.0)      ; TODO this value could be smaller
           (render-x-min (- (x camera) render-delta))
           (render-x-max (+ (x camera) (width camera) render-delta))
           (render-y-min (- (y camera) render-delta))
           (render-y-max (+ (y camera) (height camera) render-delta)))
      (flet ((in-render-area-p (game-object)
               (multiple-value-bind (x y z w h) (world-dimensions game-object)
                 (declare (ignore z)
                          (single-float x y w h))
                 (and (or (<= render-x-min x render-x-max)
                          (<= render-x-min (+ x w) render-x-max)
                          (and (<= x render-x-min)
                               (>= (+ x w) render-x-max)))
                      (or (<= render-y-min y render-y-max)
                          (<= render-y-min (+ y h) render-y-max)
                          (and (<= y render-y-min)
                               (>= (+ y h) render-y-max)))))))
        (loop :for i :from 0 :below fill-pointer :do
             (let ((object (elt objects-to-render i)))
               (when (or (not (typep object 'obb))
                         (typep object 'scene-background)
                         (in-render-area-p object))
                 (render object update-percent camera gl-context)
                 (when (log:trace)
                   (unless (typep object 'static-object)
                     (log:trace "-- ~A" object)))))))))
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
    (when (eq object1 object2)
      (log:trace "comparing the same object: ~A" object1))
    (labels ((z-layer (object)
               (cond ((and (typep object 'obb)
                           (not (null (parent object))))
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
