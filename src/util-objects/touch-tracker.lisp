(in-package :recurse.vert)

;;;; Region of a touch-tracker

(defclass touch-region (obb)
  ((touch-tracker :initarg :touch-tracker
                  :initform (error ":touch-tracker required"))
   (touching :initform (list)
             :accessor touching
             :documentation "Unordered list of objects this region is touching."))
  (:documentation "TODO"))

(defun %remove-if-not-touching (touch-region object)
  (unless (collidep touch-region object)
    (with-slots (touch-tracker touching) touch-region
      (setf touching (delete object touching)))))

(defun %add-if-touching (touch-region object)
  (with-slots (touch-tracker touching) touch-region
    (when (and (collidep touch-region object)
               (not (find object touching)))
      (add-subscriber object touch-region object-moved)
      (push object touching)
      (with-slots (all-objects-touching) touch-tracker
        (unless (find object all-objects-touching)
          (push object all-objects-touching))))))

(defmethod object-moved :after ((touch-region touch-region))
  (loop for touched-object in (touching touch-region) do
       (%remove-if-not-touching touch-region touched-object)))

(defevent-callback object-moved ((touched-object aabb) (region touch-region))
  (%remove-if-not-touching region touched-object))

;;;; Touch Tracker

(defclass touch-tracker (convex-polygon)
  ((touch-regions
    ;; defaults to four sides of obb
    :initform (list)
    :reader touch-regions
    :documentation "Locations where the touch-tracker is being touched.
Implemented as alist of :region-name to a TOUCH-REGION
May be extended or overridden by subclasses.")
   (all-objects-touching :initform (list)))
  (:documentation "An object that tracks the objects it is touching."))

@export
(defun add-touch-region (touch-tracker region-name touch-region &optional override-existing)
  (declare (touch-tracker touch-tracker)
           (symbol region-name)
           (touch-region touch-region))
  (with-slots (touch-regions) touch-tracker
    (when (eq :all region-name)
      (error ":all region-name is reserved"))
    (when (assoc region-name touch-regions)
      (if override-existing
          (setf touch-regions (delete (assoc region-name touch-regions) touch-regions))
          (error "~A region already exists" region-name)))
    (pin-to touch-region touch-tracker)
    (push (cons region-name touch-region) touch-regions)
    touch-tracker))

(defgeneric objects-touching (touch-tracker &optional region-name)
  (:documentation "Get all objects touching the REGION-NAME touch-region.
Or pass :all to get all objects touching TOUCH-TRACKER")
  (:method ((touch-tracker touch-tracker) &optional (region-name :all))
    (declare (optimize (speed 3))
             (keyword region-name))
    (if (eq :all region-name)
        (with-slots (all-objects-touching) touch-tracker
          (loop :for object :in all-objects-touching :do
            (loop :for region-cons :in (touch-regions touch-tracker) :do
              (when (find object (the list (slot-value (cdr region-cons) 'touching))) (return))
              ;; if we make it here none of the touch regions have the object
                  :finally (setf all-objects-touching (delete object all-objects-touching))))
          all-objects-touching)
        (let ((region (cdr (assoc region-name (touch-regions touch-tracker)))))
          (unless region (error "No such region: ~A" region-name))
          (touching region)))))

;; TODO: move :around width/height scaling updates to pinned-objects
(defmethod (setf width) :around (new-width (touch-tracker touch-tracker))
  (let* ((old-width (width touch-tracker))
         (result (call-next-method new-width touch-tracker))
         (new-width (width touch-tracker))
         (scaling-factor (/ new-width old-width)))
    (loop for (ignored . touch-region) in (touch-regions touch-tracker) do
         (let ((x-displacement (- (x touch-region) (x touch-tracker))))
           (when (> x-displacement 0)
             (setf x-displacement (* x-displacement scaling-factor))
             (setf (x touch-region) (+ (x touch-tracker) x-displacement))))
         (setf (width touch-region) (* (width touch-region) scaling-factor)))
    result))

(defmethod (setf height) :around (new-height (touch-tracker touch-tracker))
  (let* ((old-height (height touch-tracker))
         (result (call-next-method new-height touch-tracker))
         (new-height (height touch-tracker))
         (scaling-factor (/ new-height old-height)))
    (loop for (ignored . touch-region) in (touch-regions touch-tracker) do
         (let ((y-displacement (- (y touch-region) (y touch-tracker))))
           (when (> y-displacement 0)
             (setf y-displacement (* y-displacement scaling-factor))
             (setf (y touch-region) (+ (y touch-tracker) y-displacement))))
         (setf (height touch-region) (* (height touch-region) scaling-factor)))
    result))

(defmethod collision :after ((touch-tracker touch-tracker) (stationary-object game-object))
  (loop for (region-name . region) in (touch-regions touch-tracker) do
       (%add-if-touching region stationary-object)))

(defmethod collision :after ((stationary-object game-object) (touch-tracker touch-tracker))
  (loop for (region-name . region) in (touch-regions touch-tracker) do
       (%add-if-touching region stationary-object)))

(defmethod collision :after ((tracker1 touch-tracker) (tracker2 touch-tracker))
  (loop for (region-name . region) in (touch-regions tracker1) do
       (%add-if-touching region tracker2))
  (loop for (region-name . region) in (touch-regions tracker2) do
       (%add-if-touching region tracker1)))

;;;; OBB touch-tracker

(defclass obb-touch-tracker (touch-tracker obb)
  ()
  (:documentation "Convenience class for obb which tracks which face is being touched."))

(defmethod initialize-instance :after ((touch-tracker obb-touch-tracker) &rest args)
  (declare (ignore args))
  (with-accessors ((x x) (y y) (z z)
                   (w width) (h height) (r rotation))
      touch-tracker
    (let* ((region-extension *collision-precision*)
           (north (make-instance 'touch-region
                                 :touch-tracker touch-tracker
                                 :rotation r
                                 :x x
                                 :y (- y region-extension)
                                 :z z
                                 :width w
                                 :height region-extension))
           (east (make-instance 'touch-region
                                 :touch-tracker touch-tracker
                                :rotation r
                                :x (+ x w)
                                :y y
                                :z z
                                :width region-extension
                                :height h))
           (south (make-instance 'touch-region
                                 :touch-tracker touch-tracker
                                 :rotation r
                                 :x x
                                 :y (+ y h)
                                 :z z
                                 :width w
                                 :height region-extension))
           (west (make-instance 'touch-region
                                 :touch-tracker touch-tracker
                                :rotation r
                                :x (- x region-extension)
                                :y y
                                :z z
                                :width region-extension
                                :height h)))
      (add-touch-region touch-tracker :north north)
      (add-touch-region touch-tracker :east east)
      (add-touch-region touch-tracker :south south)
      (add-touch-region touch-tracker :west west))))
