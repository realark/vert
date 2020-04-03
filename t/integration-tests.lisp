(in-package :recurse.vert)

;;;; Scenes for various tests and benchmarks. Not run in CI.

(defclass %test-rectangle (static-sprite kinematic-object)
  ((path-to-sprite :initform (resource-path "./rectangle.png"))))

(defun profile-collision-checking ()
  "Update many moving objects on the screen, but none of the objects will actually collide."
  (when *engine-manager*
    (error "vert already running"))
  (recurse.vert:main (lambda ()
                       (let* ((test-run-time-ms 3000)
                              (run-sprof nil)
                              (run-profiler t)
                              ;; (rect-v-x 0.0)
                              (rect-v-x (float 1/32))
                              (rows 10)
                              (cols 10)
                              (rectangles (make-array rows))
                              (move-forward-p t)
                              (scene (make-instance 'platformer-game-scene
                                                    :gravity-vector (vector2 0.0 0.0)
                                                    :width (or (first (getconfig 'game-resolution *config*))
                                                               100.0)
                                                    :height (or (second (getconfig 'game-resolution *config*))
                                                                100.0)
                                                    :friction-x 0.0
                                                    :drag-y 0.0
                                                    :custom-update-fn
                                                    (let ((t0 nil))
                                                      (lambda ()
                                                        (unless t0
                                                          (setf t0 (scene-ticks *scene*))
                                                          (when run-sprof
                                                            (sb-sprof:reset)
                                                            (sb-sprof:start-profiling
                                                             :mode :alloc
                                                             :threads (list *vert-thread*)))
                                                          (when run-profiler
                                                            (sb-profile:profile vert:world-points world-dimensions complex-obb-dimensions collidep)
                                                            (sb-profile:reset))
                                                          (sleep 1))
                                                        (let ((delta (- (scene-ticks *scene*) t0)))
                                                          (loop :for rect-row :across rectangles :do
                                                               (loop :for rect :across rect-row :do
                                                                    (if move-forward-p
                                                                        (setf (velocity-x rect) rect-v-x)
                                                                        (setf (velocity-x rect) (- rect-v-x)))))
                                                          (setf move-forward-p
                                                                (not move-forward-p))
                                                          (when (> delta test-run-time-ms)
                                                            (when run-sprof
                                                              (sb-sprof:stop-profiling)
                                                              (sb-sprof:report :type :flat)
                                                              (finish-output)
                                                              (format t "~%~%"))
                                                            (when run-profiler
                                                              (sb-profile:report)
                                                              (sb-profile:unprofile))
                                                            (loop :for stats :across (game-stats *engine-manager*) :do
                                                                 (when (typep stats 'builtin-vert-stats)
                                                                   (let ((update-timer (slot-value stats 'update-timer)))
                                                                     (multiple-value-bind (avg-ns update-fps longest-avg-ns)
                                                                         (frame-timer-stats update-timer)
                                                                       (declare (ignore update-fps))
                                                                       (log:info "~%~%Update Stats:: avg: ~A wrst: ~A~%"
                                                                                 (truncate-float (/ avg-ns #.(expt 10.0 6))
                                                                                                 +default-stats-decimal-places+)
                                                                                 (truncate-float (/ longest-avg-ns #.(expt 10.0 6)) +default-stats-decimal-places+)))
                                                                     (return update-timer))))
                                                            (quit))))))))
                         (let ((parent (make-instance 'obb)))
                           (loop :for i :from 0 :below (length rectangles) :do
                                (let ((rect-row (make-array cols)))
                                  (setf (elt rectangles i) rect-row)
                                  (loop :for j :from 0 :below (length rect-row) :do
                                       (let* ((spacing 2)
                                              (w 1)
                                              (h 1)
                                              (rect (make-instance '%test-rectangle
                                                                   :parent parent
                                                                   :x (+ (* j spacing) (* j w) )
                                                                   :y (+ (* i spacing) (* i h) )
                                                                   :width w
                                                                   :height h)))
                                         (setf (elt rect-row j) rect)
                                         (add-to-scene scene rect))))))
                         scene))
                     :config *default-config*
                     :block t
                     :dev-mode (make-config ()
                                            ('dev-mode-performance-hud t))))
