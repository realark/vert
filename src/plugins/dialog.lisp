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
  ((window-width :initarg :window-width
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
