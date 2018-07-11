;; An external device that generates input.
(in-package :recurse.vert)

(defparameter *all-input-id* -1
  "ID for binding every available input. This is useful for menus or shared controls.")

(defparameter %input-id% 0)

(defclass input-device (event-publisher)
  ((input-name
    :initarg :input-name
    :initform (error ":input-name must be specified")
    :reader input-name
    :documentation "Name of the input device. E.g. :keyboard")
   (device-id
    :initform (incf %input-id%)
    :reader device-id
    :documentation "Unique id of the input device.")
   (active-inputs
    :initform (make-array 4 :fill-pointer 0 :adjustable T)
    :reader get-active-inputs
    :documentation "Inputs which are currently active")
   (deactivated-inputs
    :initform (make-array 4 :fill-pointer 0 :adjustable T)
    :reader get-deactivated-inputs
    :documentation "Inputs which were deactivated last update frame."))
  (:documentation "Single source of external input. E.g. keyboard, mouse, controller."))

(defgeneric after-input-update (input-device)
  (:documentation "Called at the end of every input update frame.")
  (:method ((input-device input-device))
    ;; reset deactivated inputs.
    (setf (fill-pointer (slot-value input-device 'deactivated-inputs)) 0)))

(defgeneric activate-input (input-device input)
  (:documentation "Activate INPUT on INPUT-DEVICE")
  (:method ((input-device input-device) (input symbol))
    (with-slots (active-inputs) input-device
      (unless (find input active-inputs)
        (vector-push-extend input active-inputs)))))

(defgeneric deactivate-input (input-device input)
  (:documentation "Deactivate INPUT on INPUT-DEVICE")
  (:method ((input-device input-device) (input symbol))
    (with-slots (active-inputs deactivated-inputs) input-device
      (setf active-inputs (delete input active-inputs))
      (unless (find input deactivated-inputs)
        (vector-push-extend input deactivated-inputs)))))

(defevent input-update ((input-device input-device))
    "Fired by each registered input device once per update frame.")
