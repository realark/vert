(in-package :recurse.vert)

(defcollision-resolution linear-resolution ((moving-object kinematic-object)
                                            (stationary-object obb)
                                            &key original-position)
  (declare (optimize (speed 3))
           (ftype (function (vector3 vector3) world-position) distance-between))
  (let ((colliding-point (vector3 0.0 0.0 0.0))
        (non-colliding-point (vector3 0.0 0.0 0.0)))
    (declare (dynamic-extent colliding-point non-colliding-point))
    (setf (x colliding-point) (x moving-object)
          (y colliding-point) (y moving-object)
          (z colliding-point) (z moving-object))
    (setf (x non-colliding-point) (x original-position)
          (y non-colliding-point) (y original-position)
          (z non-colliding-point) (z original-position))
    ;; zero out v and a on collision axis
    (let ((x-axis-collision nil)
          (y-axis-collision nil)
          (favored-axis (favored-collision-resolution-axis (hit-box moving-object stationary-object)
                                                           stationary-object)))
      (setf (y moving-object) (y non-colliding-point))
      (unless (collidep moving-object stationary-object)
        (setf y-axis-collision T))
      (setf (y moving-object) (y colliding-point))
      (setf (x moving-object) (x non-colliding-point))
      (unless (collidep moving-object stationary-object)
        (setf x-axis-collision T))
      (setf (x moving-object) (x colliding-point))
      (when (and x-axis-collision y-axis-collision (not (null favored-axis)))
        (if (eq favored-axis 'x)
            (setf x-axis-collision nil)
            (setf y-axis-collision nil)))
      (cond
        (y-axis-collision
         (log:trace "killing y-axis movement for ~A" moving-object)
         (setf (velocity-y moving-object) 0.0
               (acceleration-y moving-object) 0.0
               (x non-colliding-point) (x colliding-point)))
        (x-axis-collision
         (log:trace "killing x-axis movement for ~A" moving-object)
         (setf (velocity-x moving-object) 0.0
               (acceleration-x moving-object) 0.0
               (y non-colliding-point) (y colliding-point)))
        (T
         ;; neither axis is "responsible" for the collision.
         ;; Kill all velocity/acceleration
         (log:trace "killing movement along x and y axis for ~A" moving-object)
         (setf (velocity-y moving-object) 0.0
               (acceleration-y moving-object) 0.0
               (velocity-x moving-object) 0.0
               (acceleration-x moving-object) 0.0))))
    ;; find closest non-colliding point
    ;; and move object to it
    (loop :while (> (distance-between colliding-point non-colliding-point)
                    (the world-position *collision-precision*))
       :do (setf
            ;; move object halfway between the two points
            (x moving-object) (/ (+ (x non-colliding-point) (x colliding-point)) 2.0)
            (y moving-object) (/ (+ (y non-colliding-point) (y colliding-point)) 2.0)
            (z moving-object) (/ (+ (z non-colliding-point) (z colliding-point)) 2.0))
         (if (collidep moving-object stationary-object)
             (setf (x colliding-point) (x moving-object)
                   (y colliding-point) (y moving-object)
                   (z colliding-point) (z moving-object))
             (setf (x non-colliding-point) (x moving-object)
                   (y non-colliding-point) (y moving-object)
                   (z non-colliding-point) (z moving-object)))
       :finally (setf (x moving-object) (x non-colliding-point)
                      (y moving-object) (y non-colliding-point)
                      (z moving-object) (z non-colliding-point)))))

(defmotion linear-motion ((object kinematic-object) delta-t-ms (physics-context physics-context-2d))
  (declare (optimize (speed 3))
           ((integer 1 100) delta-t-ms))
  (flet ((cap (max-magnitude x)
           (declare (single-float x)
                    ((or null vector-dimension) max-magnitude))
           (if max-magnitude
               (if (<= 0 x)
                   (min x max-magnitude)
                   (max x (- max-magnitude)))
               x)))
    (declare (inline cap))
    (let ((original-position (vector3 0.0 0.0 0.0)))
      (declare (dynamic-extent original-position))
      (with-accessors ((max-v-x max-velocity-x) (max-v-y max-velocity-y)
                       (friction-x friction-x) (drag-y drag-y))
          physics-context
        (declare ((single-float 0.0 1.0) friction-x drag-y))
        (with-accessors ((x x) (y y) (z z)
                         (v-x velocity-x) (v-y velocity-y)
                         (a-x acceleration-x) (a-y acceleration-y))
            object
          (declare (world-position x y z)
                   (vector-dimension v-x v-y a-x a-y))
          (block zero-out-velocity
            (when (and (/= 0f0 v-x)
                   (= 0f0 a-x)
                   (< 0f0 (abs v-x) *movement-threshold*))
              (setf v-x 0f0))
            (when (and (/= 0f0 v-y)
                   (= 0f0 a-y)
                   (< 0f0 (abs v-y) *movement-threshold*))
              (setf v-y 0f0)))
          (block update-position
            (unless (= 0.0 v-x v-y)
              (setf (x original-position) x
                    (y original-position) y
                    (z original-position) z)
              (with-collision-check (object physics-context)
                (:position-update
                 (log:trace "moving (~A,~A) -> ~A" v-x v-y object)
                 ;; for some reason, the compiler complains if I use incf
                 (setf x (+ x (* v-x delta-t-ms)))
                 (setf y (+ y (* v-y delta-t-ms))))
                (:on-collision stationary-object
                               (linear-resolution object
                                                  stationary-object
                                                  :original-position original-position)))
              ;; apply friction and drag
              (setf v-x (* (expt friction-x delta-t-ms) v-x)
                    v-y (* (expt drag-y delta-t-ms) v-y)
                    x (round x)
                    y (round y))))
          (block update-velocity-and-acceleration
            (unless (= 0.0 v-x v-y a-x a-y)
              (setf
               ;; update velocity
               v-x (cap max-v-x (+ v-x (* a-x delta-t-ms)))
               v-y (cap max-v-y (+ v-y (* a-y delta-t-ms)))
               ;; update acceleration
               a-x 0.0
               a-y 0.0)))))))
  (values))
