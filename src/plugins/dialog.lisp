(in-package :recurse.vert)

(defclass dialog-speaker (game-object)
  ()
  (:documentation "A game object which my product dialog."))

;; TODO
;; (defgeneric dialog-speaker-name (dialog-speaker)
;;   (:documentation "The name to show when DIALOG-SPEAKER is speaking. Maybe be nil."))

;; (defgeneric dialog-speaker-picture (dialog-speaker)
;;   (:documentation "Picture to show when DIALOG-SPEAKER is speaking. (values path-to-image sprite-source). Maybe be nil."))

;; (defgeneric dialog-speaker-sfx (dialog-speaker)
;;   (:documentation "Sound to play when DIALOG-SPEAKER is speaking. Maybe be nil."))

(defclass dialog-hud (overlay input-handler)
  ((show-p :initform nil)
   (initiator :initform nil
              :documentation "game-object to return input control to once this HUD closes.")
   (window-width :initarg :window-width
                 :initform 100)
   (window-height :initarg :window-height
                  :initform 10)
   (window-padding :initarg :window-padding
                   :initform 1
                   :documentation "Guaranteed amount of space on all sides of the window which will not contain text.")
   (text :initarg :text
         :type font-drawable
         :initform nil
         :reader dialog-hud-text)
   (background :initarg :background
               :initform nil)))

(defmethod initialize-instance :after ((hud dialog-hud) &rest args)
  (declare (ignore args))
  (with-slots (window-width window-height window-padding text background) hud
    (unless text
      (error ":text required"))
    (let ((text-width (- window-width (* 2 window-padding)))
          (text-height (- window-height (* 2 window-padding))))
      (when background
        ;; add background first so it renders behind the text
        (setf (parent background) hud))
      (setf (parent text) hud
            (x text) (+ (- (/ (width hud) 2.0)
                           (/ text-width 2.0))
                        window-padding)
            (y text) window-padding
            (width text) text-width
            (height text) text-height)
      (when background
        (setf (width background) window-width
              (height background) window-height
              (x background) (- (x text) window-padding)
              (y background) (- (y text) window-padding))))))

(defmethod render ((hud dialog-hud) update-percent camera rendering-context)
  (when (slot-value hud 'show-p)
    (call-next-method hud update-percent camera rendering-context)))

(defun advance-dialog (dialog-hud)
  (declare (dialog-hud dialog-hud))
  ;; TODO: advance to next block of dialog when one exists
  (quit-dialog dialog-hud))

(defun quit-dialog (dialog-hud)
  (declare (dialog-hud dialog-hud))
  (with-slots (show-p initiator) dialog-hud
    (when initiator
      (setf (active-input-device initiator) (active-input-device dialog-hud)))
    (setf (active-input-device dialog-hud) *no-input-id*
          show-p nil)))

(defun show-dialog (dialog-hud text &key initiator)
  (with-slots (show-p (hud-initiator initiator)) dialog-hud
    (when show-p (error "dialog already shown"))
    (when initiator
      (setf (active-input-device dialog-hud) (active-input-device initiator)
            (active-input-device initiator) *no-input-id*))
    (setf hud-initiator initiator
          (text (dialog-hud-text dialog-hud)) text
          show-p t)))

(set-default-input-command-map
 dialog-hud
 ("controller"
  (:0 :advance-dialog))
 ("sdl-keyboard"
  (:scancode-space :advance-dialog)))

(set-default-command-action-map
 dialog-hud
 (:advance-dialog
  (on-activate (advance-dialog dialog-hud))))
