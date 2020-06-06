(in-package :recurse.vert)

;;;; Region of a touch-tracker

(defclass touch-region (obb phantom)
  ((touch-tracker :initarg :touch-tracker
                  :initform (error ":touch-tracker required"))
   (touching :initform (make-array 0
                                   :element-type 'game-object
                                   :fill-pointer 0
                                   :adjustable t)
             :accessor touching
             :documentation "Unordered list of objects this region is touching."))
  (:documentation "Subscction of a touch-tracker."))

(defun %remove-if-not-touching (touch-region object)
  (unless (collidep touch-region object)
    (with-slots (touch-tracker touching) touch-region
      (setf touching (delete object touching))
      (remove-subscriber object touch-region object-moved)
      (%refresh-all-objects-touching touch-tracker))))

(defun %add-if-touching (touch-region object)
  (with-slots (touch-tracker touching) touch-region
    (when (and (not (find object touching))
               (collidep touch-region object))
      (unless (typep object 'static-object)
        (add-subscriber object touch-region object-moved))
      (vector-push-extend object touching)
      (log:trace "~A : add object ~A" touch-region object)
      (with-slots (all-objects-touching) touch-tracker
        (unless (find object all-objects-touching)
          (vector-push-extend object all-objects-touching))))))

(defmethod object-moved :after ((touch-region touch-region))
  (loop :for touched-object :across (touching touch-region) do
       (%remove-if-not-touching touch-region touched-object)))

(defevent-callback object-moved ((touched-object obb) (region touch-region))
  (%remove-if-not-touching region touched-object))

;;;; Touch Tracker

(defclass touch-tracker ()
  ((touch-regions
    :initform (make-array 0
                          :fill-pointer 0
                          :adjustable t)
    :reader touch-regions
    :documentation "p-vector #(:region1-name touch-region1 region2-name touch-region2 ...)")
   (all-objects-touching :initform (make-array 0
                                               :element-type 'game-object
                                               :fill-pointer 0
                                               :adjustable t)))
  (:documentation "An object that tracks the objects it is touching."))

(defun %get-touch-region (region-name touch-tracker)
  (with-slots (touch-regions) touch-tracker
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (when (equalp region-name (elt touch-regions i))
           (return (elt touch-regions (+ i 1)))))))

(defun %refresh-all-objects-touching (touch-tracker)
  "Rebuild all-objects-touching array from scratch"
  (log:trace "~A refresh all objects touching" touch-tracker)
  (with-slots (touch-regions all-objects-touching) touch-tracker
    (setf (fill-pointer all-objects-touching) 0)
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (loop :for object :across (touching (elt touch-regions (+ i 1))) :do
              (unless (find object all-objects-touching)
                (vector-push-extend object all-objects-touching))))))

@export
(defun add-touch-region (touch-tracker region-name touch-region &optional override-existing)
  (declare (touch-tracker touch-tracker)
           (symbol region-name)
           (touch-region touch-region))
  (when (eq :all region-name)
    (error ":all region-name is reserved"))
  (with-slots (touch-regions) touch-tracker
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (when (equalp region-name (elt touch-regions i))
           (if override-existing
               (setf (elt touch-regions (+ i 1)) touch-region)
               (error "~A region already exists" region-name))
           (return))
       :finally
         (vector-push-extend region-name touch-regions)
         (vector-push-extend touch-region touch-regions)))
  touch-tracker)

@export
(defun objects-touching (touch-tracker &optional (region-name :all))
  "Get all objects touching the REGION-NAME touch-region. Or pass :all to get all objects touching TOUCH-TRACKER"
  (declare (optimize (speed 3))
           (touch-tracker touch-tracker)
           (keyword region-name))
  (if (eq :all region-name)
      (with-slots (all-objects-touching) touch-tracker
        (when (> (length all-objects-touching) 0)
          all-objects-touching))
      (let ((region (%get-touch-region region-name touch-tracker)))
        (unless region (error "No such region: ~A" region-name))
        (when (> (length (touching region)) 0)
          (touching region)))))

(defmethod object-moved :after ((touch-tracker touch-tracker))
  (with-slots (touch-regions) touch-tracker
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (object-moved (elt touch-regions (+ i 1))))))

;; TODO: move :around width/height scaling updates to pinned-objects
(defmethod (setf width) :around (new-width (touch-tracker touch-tracker))
  (let* ((old-width (width touch-tracker))
         (result (call-next-method new-width touch-tracker))
         (new-width (width touch-tracker))
         (scaling-factor (/ new-width old-width)))
    (with-slots (touch-regions) touch-tracker
      (loop :for i :from 0 :below (length touch-regions) :by 2 :do
           (let ((touch-region (elt touch-regions (+ i 1))))
             (let ((x-displacement (- (x touch-region) (x touch-tracker))))
               (when (> x-displacement 0)
                 (setf x-displacement (* x-displacement scaling-factor))
                 (setf (x touch-region) (+ (x touch-tracker) x-displacement))))
             (setf (width touch-region) (* (width touch-region) scaling-factor)))))
    result))

(defmethod (setf height) :around (new-height (touch-tracker touch-tracker))
  (let* ((old-height (height touch-tracker))
         (result (call-next-method new-height touch-tracker))
         (new-height (height touch-tracker))
         (scaling-factor (/ new-height old-height)))
    (with-slots (touch-regions) touch-tracker
      (loop :for i :from 0 :below (length touch-regions) :by 2 :do
           (let ((touch-region (elt touch-regions (+ i 1))))
             (let ((y-displacement (- (y touch-region) (y touch-tracker))))
               (when (> y-displacement 0)
                 (setf y-displacement (* y-displacement scaling-factor))
                 (setf (y touch-region) (+ (y touch-tracker) y-displacement))))
             (setf (height touch-region) (* (height touch-region) scaling-factor)))))
    result))

(defmethod collision :after ((touch-tracker touch-tracker) (stationary-object game-object))
  (with-slots (touch-regions) touch-tracker
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (%add-if-touching (elt touch-regions (+ i 1)) stationary-object))))

(defmethod collision :after ((other-object game-object) (touch-tracker touch-tracker))
  (with-slots (touch-regions) touch-tracker
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (%add-if-touching (elt touch-regions (+ i 1)) other-object))))

(defmethod collision :after ((tracker1 touch-tracker) (tracker2 touch-tracker))
  (with-slots (touch-regions) tracker1
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (%add-if-touching (elt touch-regions (+ i 1)) tracker2)))
  (with-slots (touch-regions) tracker2
    (loop :for i :from 0 :below (length touch-regions) :by 2 :do
         (%add-if-touching (elt touch-regions (+ i 1)) tracker1))))

;;;; OBB touch-tracker

(defclass obb-touch-tracker (touch-tracker obb)
  ()
  (:documentation "Convenience class for obb which tracks which face is being touched."))

(defmethod initialize-instance :after ((touch-tracker obb-touch-tracker) &rest args)
  (declare (ignore args))
  (with-accessors ((x x) (y y) (z z)
                   (w width) (h height) (r rotation))
      touch-tracker
    (let* ((region-extension (* 2.0 *collision-precision*))
           (north (make-instance 'touch-region
                                 :touch-tracker touch-tracker
                                 :parent touch-tracker
                                 :x 0
                                 :y (- region-extension)
                                 :z 0
                                 :width w
                                 :height region-extension))
           (east (make-instance 'touch-region
                                :touch-tracker touch-tracker
                                :parent touch-tracker
                                :x w
                                :y 0
                                :z 0
                                :width region-extension
                                :height h))
           (south (make-instance 'touch-region
                                 :touch-tracker touch-tracker
                                :parent touch-tracker
                                 :x 0
                                 :y h
                                 :z 0
                                 :width w
                                 :height region-extension))
           (west (make-instance 'touch-region
                                :touch-tracker touch-tracker
                                :parent touch-tracker
                                :x (- region-extension)
                                :y 0
                                :z 0
                                :width region-extension
                                :height h)))
      (add-touch-region touch-tracker :north north)
      (add-touch-region touch-tracker :east east)
      (add-touch-region touch-tracker :south south)
      (add-touch-region touch-tracker :west west))))
