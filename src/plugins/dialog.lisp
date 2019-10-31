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
  ((text-width :initarg :text-width
               :initform 100)
   (text-height :initarg :text-height
                :initform 10)
   (border-padding :initarg :border-padding
                   :initform 1)
   (text :initarg :text-color
         :initform nil)
   (background :initarg :background
               :initform nil)
   (text-color :initform *black*)))

(defmethod initialize-instance :after ((hud dialog-hud) &rest args)
  (declare (ignore args))
  (with-slots (background border-padding text text-color text-width text-height) hud
    (when background
      ;; add background first so it renders behind the text
      (setf (parent background) hud))
    (setf text
          (make-instance 'font-drawable
                         :color text-color
                         :parent hud
                         :x (- (/ (width hud) 2.0)
                               (/ text-width 2.0))
                         :y 0
                         :width text-width
                         :height text-height
                         :text "A man, a plan, a canal. Panama!"))
    (when background
      (setf (width background) (+ (width text) border-padding)
            (height background) (+ (height text) border-padding)
            (x background) (- (x text) border-padding)
            (y background) (- (y text) border-padding)))))
