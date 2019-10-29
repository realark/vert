;;;; Direction tracking agent

(in-package :recurse.vert)

@export
(defclass direction-tracker (agent obb)
  ((facing :initarg :facing
           :initform (make-array 2
                                 :adjustable nil
                                 :initial-contents (list :EAST nil))
           :reader facing)
   (simultanious-directions-p :initarg :simultainous-directions-p
                              :initform t))
  (:documentation "An agent that tracks the directions it is facing on a 2d grid (North, South, East, West)."))

@export
(defun push-direction (direction-tracker direction)
  "Face DIRECTION-TRACKER towards DIRECTION. Mutually exclusive dirs will be updated (e.g. pushing :east removes :west)."
  (declare (direction-tracker direction-tracker)
           (keyword direction))
  (flet ((opposite-direction (direction)
           (case direction
             (:NORTH :SOUTH)
             (:SOUTH :NORTH)
             (:EAST :WEST)
             (:WEST :EAST)
             (otherwise nil)))
         (assert-valid-direction (direction)
           (case direction
             (:NORTH nil)
             (:SOUTH nil)
             (:EAST nil)
             (:WEST nil)
             (otherwise (error "~A is not a valid direction" direction)))))
    (assert-valid-direction direction)
    (with-slots (facing simultanious-directions-p) direction-tracker
      (unless (find direction facing)
        (if simultanious-directions-p
            (let ((opposite (opposite-direction direction)))
              ;; first try to replace the opposite direction
              (loop :for i :from 0 :below (length facing) :do
                   (when (equal opposite (elt facing i))
                     (setf (elt facing i) direction)
                     (return))
                 :finally
                   ;; no opposite to replace. Just put in the vector
                   (loop :for i :from 0 :below (length facing) :do
                        (when (null (elt facing i))
                          (setf (elt facing i) direction)
                          (return))
                      :finally
                        (error "Unable to insert ~A into facing vector ~A" direction facing))))
            ;; only one direction is allowed so just replace the first element
            (setf (first facing) direction))))))

@export
(defun facing-p (agent object-or-direction-keyword)
  "T if the AGENT is facing OBJECT"
  (declare (direction-tracker agent))
  (with-accessors ((facing facing)) agent
    (cond ((keywordp object-or-direction-keyword)
           (find object-or-direction-keyword facing))
          (t (let ((delta-x (- (x agent) (x object-or-direction-keyword)))
                   (delta-y (- (y agent) (y object-or-direction-keyword))))
               (or (and (find :NORTH facing) (<= 0 delta-y))
                   (and (find :SOUTH facing) (>= 0 delta-y))
                   (and (find :WEST facing) (<= 0 delta-x))
                   (and (find :EAST facing) (>= 0 delta-x))))))))
