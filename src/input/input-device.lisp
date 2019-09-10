;; An external device that generates input.
(in-package :recurse.vert)

@export
(defparameter *all-input-id* -1
  "ID for binding every available input. This is useful for menus or shared controls.")

@export
(defparameter *no-input-id* -2
  "ID for binding every no inputs. Used to disable input (for example, while playing a death animation).")

(defparameter %next-input-id% 0)

(defclass input-device (event-publisher)
  ((input-name
    :initarg :input-name
    :initform (error ":input-name must be specified")
    :reader input-name
    :documentation "Name of the input device. E.g. :keyboard")
   (device-id
    :initform (incf %next-input-id%)
    :reader device-id
    :documentation "Unique id of the input device.")
   (activated-inputs
    :initform (make-array 4 :fill-pointer 0 :adjustable T)
    :reader get-activated-inputs
    :documentation "Inputs which have just gone active for the first time this frame.")
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
    ;; TODO: move all activated -> active
    (with-slots (activated-inputs active-inputs deactivated-inputs) input-device
      (loop :for new-input :across activated-inputs :do
           (vector-push-extend new-input active-inputs))
      ;; reset activated and deactivated inputs.
      (setf (fill-pointer activated-inputs) 0
            (fill-pointer deactivated-inputs) 0))))

(defgeneric activate-input (input-device input)
  (:documentation "Activate INPUT on INPUT-DEVICE")
  (:method ((input-device input-device) (input symbol))
    (with-slots (activated-inputs active-inputs) input-device
      (unless (or (find input activated-inputs)
                  (find input active-inputs))
        (vector-push-extend input activated-inputs)))))

(defgeneric deactivate-input (input-device input)
  (:documentation "Deactivate INPUT on INPUT-DEVICE")
  (:method ((input-device input-device) (input symbol))
    (with-slots (activated-inputs active-inputs deactivated-inputs) input-device
      (setf activated-inputs (delete input activated-inputs))
      (setf active-inputs (delete input active-inputs))
      (unless (find input deactivated-inputs)
        (vector-push-extend input deactivated-inputs)))))

(defevent input-update ((input-device input-device))
    "Fired by each registered input device once per update frame.")
