;;; global input system. Manages registered input devices
(in-package :recurse.vert)

;; ----Terms----
;; input                        == raw commands originating from outside the engine (a key press, mouse movement, etc)
;; input-device                 == a device that generates input. A keyboard, mouse, controller, etc.
;; command                      == High-level command originating from outside the world. Example: :move-right.
;; input-command-map            == Map of device-name -> input-device -> (input -> action)*
;; active-(input|command)       == An input or command which is currently entering the system
;; deactivated-(input|command)  == An input or command which was active, but has stopped. This will be active one update frame after the input ceases.

;; example: A keyboard input-device generates a :right-arrow input. The input is looked up in the action map, and :move-right is found. The player on the screen moves right.

(defclass input-manager ()
  ((input-devices :initform (make-array 4 :fill-pointer 0 :adjustable T)
                  :reader input-devices))
  (:documentation "Manages all input-devices connected to the game engine."))

(defgeneric register-input-device (input-manager input-device)
  (:documentation "Register INPUT-DEVICE with INPUT-MANAGER")
  (:method ((input-manager input-manager) (new-device input-device))
    (with-accessors ((input-devices input-devices)) input-manager
      (loop for registered-device across input-devices do
           (when (= (device-id new-device)
                    (device-id registered-device))
             (error (format nil
                            "device ~A already registered"
                            new-device))))
      (vector-push-extend new-device input-devices)
      new-device)))

(defmacro do-input-devices (input-device-binding input-manager &body body)
  `(loop for ,input-device-binding across (input-devices ,input-manager) do
        ,@body))

(defgeneric get-input-devices (input-manager input-device-name)
  (:documentation "Return an array of all registered input devices named INPUT-DEVICE-NAME.")
  (:method ((input-manager input-manager) input-device-name)
    (let ((devices (list)))
      (do-input-devices device input-manager
        (when (equalp (input-name device) input-device-name)
          (push device devices)))
      devices)))
