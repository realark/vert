;; An external device that generates input.
(in-package :recurse.vert)

@export
(defparameter *all-input-id* -1
  "ID for binding every available input. This is useful for menus or shared controls.")

@export
(defparameter *no-input-id* nil
  "ID for binding every no inputs. Used to disable input (for example, while playing a death animation).")

(defparameter %next-input-id% 0)

(defparameter *generic-human-readable-names*
  (let ((htable (make-hash-table :test #'eq)))
    (loop :for button-id :from 0 :below 15 :do
         (setf (gethash (alexandria:make-keyword (write-to-string button-id))
                        htable)
               (sdl2-ffi.functions:sdl-game-controller-get-string-for-button button-id)))
    htable))

(defparameter *keyboard-human-readable-names*
  (let ((htable (make-hash-table :test #'eq)))
    (loop :for button-id :from 0 :below 15 :do
         (setf (gethash (alexandria:make-keyword (write-to-string button-id))
                        htable)
               (case button-id
                 (2 "keyboard-z")
                 (otherwise
                  (sdl2-ffi.functions:sdl-game-controller-get-string-for-button button-id)))))
    htable))

(defparameter *dualshock-human-readable-names*
  (let ((htable (make-hash-table :test #'eq)))
    ;; start with generic buttons
    (loop :for button-id :from 0 :below 15 :do
         (setf (gethash (alexandria:make-keyword (write-to-string button-id))
                        htable)
               (sdl2-ffi.functions:sdl-game-controller-get-string-for-button button-id)))
    ;; dualshock specific buttons
    (loop :for (button-id human-readable-name)
       :on '(0 "X"
             1 "Circle"
             2 "Square"
             3 "Triangle")
       :by #'cddr :while human-readable-name :do
         (setf (gethash (alexandria:make-keyword (write-to-string button-id))
                        htable)
               human-readable-name))
    htable))

(defclass input-device (event-publisher)
  ((input-name
    :initarg :input-name
    :initform (error ":input-name must be specified")
    :reader input-name
    :documentation "Name of the input device. E.g. \"Dualshock controller\"")
   (type
    :initarg :input-type
    :initform (error ":input-type required")
    :reader input-device-type
    :documentation "One of: :controller, :keyboard")
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
    :documentation "Inputs which were deactivated last update frame.")
   (human-readable-input-names
    :initform nil
    :documentation "Map of :SDL-BUTTON-OR-JOYSTICK-KEYWORD -> human-readable-string"))
  (:documentation "Single source of external input. E.g. keyboard, mouse, controller."))

;; TODO: update input-device accessors/readers to standard slot values
@export
(defmethod input-device-name ((input-device input-device))
  (slot-value input-device 'input-name))

@export
(defmethod input-device-type ((input-device input-device))
  (slot-value input-device 'type))

@export
(defmethod input-device-id ((input-device input-device))
  (slot-value input-device 'device-id))

(defmethod initialize-instance :after ((input-device input-device) &rest args)
  (declare (ignore args))
  ;; TODO: generalize human-readable input name mapping and expose config option(s)
  (flet ((set-human-readable-map (input-device)
           (with-slots (input-name human-readable-input-names) input-device
             (setf human-readable-input-names
                   (cond
                     ((eq :keyboard (input-device-type input-device))
                      *keyboard-human-readable-names*)
                     ((or (equalp "PS4 Controller" input-name)
                          (equalp "PS3 Controller" input-name))
                      *dualshock-human-readable-names*)
                     (t *generic-human-readable-names*))))))
    (set-human-readable-map input-device)))

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

@export
(defun input-device-get-human-readable-name (input-device button-id)
  (gethash button-id
           (slot-value input-device
                       'human-readable-input-names)))
