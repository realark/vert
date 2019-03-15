(in-package :recurse.vert)

(let ((colliding-point (make-point))
      (non-colliding-point (make-point)))
  (defcollision-resolution linear-resolution ((moving-object kinematic-object)
                                              (stationary-object AABB)
                                              &key original-position)
    (declare (optimize (speed 3))
             (ftype (function (point point) world-position) distance-between))
    (setf (point-x colliding-point) (x moving-object)
          (point-y colliding-point) (y moving-object)
          (point-z colliding-point) (z moving-object))
    (setf (point-x non-colliding-point) (point-x original-position)
          (point-y non-colliding-point) (point-y original-position)
          (point-z non-colliding-point) (point-z original-position))
    ;; zero out v and a on collision axis
    (let ((x-axis-collision nil)
          (y-axis-collision nil))
      (setf (y moving-object) (point-y non-colliding-point))
      (unless (collidep moving-object stationary-object)
        (setf y-axis-collision T))
      (setf (y moving-object) (point-y colliding-point))
      (setf (x moving-object) (point-x non-colliding-point))
      (unless (collidep moving-object stationary-object)
        (setf x-axis-collision T))
      (setf (x moving-object) (point-x colliding-point))
      (cond
        ((and x-axis-collision y-axis-collision)
         ;; when both x and y collide we can't zero out either axis of collision
         nil)
        (y-axis-collision
         (setf (velocity-y moving-object) 0
               (acceleration-y moving-object) 0
               (point-x non-colliding-point) (point-x colliding-point)))
        (x-axis-collision
         (setf (velocity-x moving-object) 0
               (acceleration-x moving-object) 0
               (point-y non-colliding-point) (point-y colliding-point)))
        (T
         ;; neither axis is "responsible" for the collision.
         ;; Kill all velocity/acceleration
         (setf (velocity-y moving-object) 0.0
               (acceleration-y moving-object) 0.0
               (velocity-x moving-object) 0.0
               (acceleration-x moving-object) 0.0))))
    ;; find closest non-colliding point
    ;; and move object to it
    (loop while (> (distance-between colliding-point non-colliding-point)
                   (the world-position *collision-precision*))
       do (setf
           ;; move object halfway between the two points
           (x moving-object) (/ (+ (point-x non-colliding-point) (point-x colliding-point)) 2.0)
           (y moving-object) (/ (+ (point-y non-colliding-point) (point-y colliding-point)) 2.0)
           (z moving-object) (/ (+ (point-z non-colliding-point) (point-z colliding-point)) 2.0))
         (if (collidep moving-object stationary-object)
             (setf (point-x colliding-point) (x moving-object)
                   (point-y colliding-point) (y moving-object)
                   (point-z colliding-point) (z moving-object))
             (setf (point-x non-colliding-point) (x moving-object)
                   (point-y non-colliding-point) (y moving-object)
                   (point-z non-colliding-point) (z moving-object)))
       finally (setf (x moving-object) (point-x non-colliding-point)
                     (y moving-object) (point-y non-colliding-point)
                     (z moving-object) (point-z non-colliding-point)))))

(let ((original-position (make-point)))
  (defmotion linear-motion ((object kinematic-object) delta-t-ms (physics-context physics-context-2d))
    (declare (optimize (speed 3))
             ((integer 1 100) delta-t-ms))
    ;; update position
    (unless (= 0.0
               ;; using the accessor conses so we'll check
               ;; the slot directly for the fast case
               (the vector-dimension (slot-value object 'velocity-x))
               (the vector-dimension (slot-value object 'velocity-y)))
      (with-accessors ((x x) (y y) (z z)
                       (v-x velocity-x) (v-y velocity-y))
          object
        (declare (world-position x y z)
                 (vector-dimension v-x v-y))
        (setf (point-x original-position) x
              (point-y original-position) y
              (point-z original-position) z)
        (with-motion-lock object
          (with-collision-check (object physics-context)
            (:position-update
             ;; for some reason, the compiler complains if I use incf
             (setf x (+ x (* v-x delta-t-ms)))
             (setf y (+ y (* v-y delta-t-ms))))
            (:on-collision stationary-object
                           (linear-resolution object
                                              stationary-object
                                              :original-position original-position))))))
    (unless (= 0.0
               (the vector-dimension (slot-value object 'velocity-x))
               (the vector-dimension (slot-value object 'velocity-y))
               (the vector-dimension (slot-value object 'acceleration-x))
               (the vector-dimension (slot-value object 'acceleration-y)))
      (with-accessors ((v-x velocity-x) (v-y velocity-y)
                       (a-x acceleration-x) (a-y acceleration-y))
          object
        (declare (vector-dimension v-x v-y a-x a-y))
        (with-accessors ((max-v-x max-velocity-x) (max-v-y max-velocity-y)
                         (friction-x friction-x) (drag-y drag-y))
            physics-context
          (declare ((single-float 0.0 1.0) friction-x drag-y))
          (flet ((cap (max-magnitude x)
                   (declare (single-float x))
                   (if max-magnitude
                       (locally (declare (type vector-dimension max-magnitude))
                         (if (<= 0 x)
                             (min x max-magnitude)
                             (max x (- max-magnitude))))
                       x)))
            (setf
             ;; update velocity
             v-x (cap max-v-x (+ ;; friction applied to previous x velocity
                               (* (expt friction-x delta-t-ms) v-x)
                               (* a-x delta-t-ms)))
             v-y (cap max-v-y (+ (* (expt drag-y delta-t-ms) v-y)
                                 (* a-y delta-t-ms)))
             ;; update acceleration
             a-x 0.0
             a-y 0.0)))))
    (values)))
