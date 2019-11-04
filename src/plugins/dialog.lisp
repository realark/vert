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

@export
(defclass dialog-hud (overlay input-handler)
  ((show-p :initform nil)
   (initiator :initform nil
              :documentation "game-object to return input control to once this HUD closes.")
   (window-position :initarg :window-position
                    :initform nil)
   (window-size :initarg :window-size
                :initform nil)
   (window-padding :initarg :window-padding
                   :initform 1
                   :documentation "Guaranteed amount of space on all sides of the window which will not contain text.")
   (speaker-name :initform nil)
   (text :initarg :dialog-text
         :type string
         :initform "")
   (color :initarg :dialog-color
          :initform nil)
   (font-size :initarg :dialog-font-size
              :initform 8)
   (lines :initform (make-array 0
                                :adjustable t
                                :fill-pointer 0
                                :element-type 'font-drawable))
   (background :initarg :background
               :initform nil)
   (advance-delay :initarg :advance-delay
                  :initform 500
                  :documentation "time (ms) before player is allowed to advance the dialog.")))

(defmethod initialize-instance :after ((hud dialog-hud) &rest args)
  (declare (ignore args))
  (with-slots (window-position window-size window-padding background font-size) hud
    (unless window-size
      (setf window-size (vector2 100.0 10.0)))
    (unless window-position
      (setf window-position
            (vector2
             (- (/ (width hud) 2.0)
                (/ (x window-size) 2.0))
             0.0)))
    (when background
      (setf (parent background) hud
            (width background) (x window-size)
            (height background) (y window-size)
            (x background) (x window-position)
            (y background) (y window-position)))))

(defmethod render ((hud dialog-hud) update-percent camera rendering-context)
  (when (slot-value hud 'show-p)
    (call-next-method hud update-percent camera rendering-context)))

(defun advance-dialog (dialog-hud)
  (declare (dialog-hud dialog-hud))
  ;; TODO: advance to next block of dialog when one exists
  (quit-dialog dialog-hud))

(defun quit-dialog (dialog-hud)
  (declare (dialog-hud dialog-hud))
  (with-slots (show-p initiator speaker-name) dialog-hud
    (when initiator
      (setf (active-input-device initiator) (active-input-device dialog-hud)))
    (setf (active-input-device dialog-hud) *no-input-id*
          speaker-name nil
          show-p nil)))

(defun %set-dialog-lines (dialog-hud)
  "Update hud's font-drawables to match the text and speaker content."
  (declare (dialog-hud dialog-hud))
  ;; split text content into
  (with-slots (text) dialog-hud
    (let ((font-draw (make-instance 'font-drawable
                                    ;; temporary font drawable to measure potential line lengths
                                    :text ""
                                    :font-size (slot-value dialog-hud 'font-size)))

          ;; we'll likely want to alter text for display purposes (e.g. add a hyphen for line breaks)
          ;; copy the text so the underlying content is unchanged
          (text (copy-seq text)))
      (load-resources font-draw *gl-context*)
      (unwind-protect
           (labels ((space-p (index)
                      "t if INDEX points to empty space"
                      (or (>= index (length text))
                          (< index 0)
                          (equal (elt text index) #\ )))
                    (move-index-to-after-current-word (index)
                      "Move INDEX to point to one after the current word."
                      (loop :while (and (< index (length text))
                                        (not (space-p index))) :do
                           (incf index)
                         :finally
                           (return index)))
                    (move-index-to-start-of-next-word (index)
                      "Move INDEX to point to the first char of the next word (i.e. eat whitespace)."
                      (loop :while (and (< index (length text))
                                        (space-p index)) :do
                           (incf index)
                         :finally
                           (return index)))
                    (compute-current-line-ending (current-line-beginning dialog-hud)
                      (loop :with line-ending = current-line-beginning
                         :while (< line-ending (length text))
                         :do
                           (let ((previous-word line-ending))
                             (setf line-ending (move-index-to-start-of-next-word line-ending))
                             (setf line-ending (move-index-to-after-current-word line-ending))
                             (setf (text font-draw)
                                   (subseq text current-line-beginning line-ending))
                             (when (> (width font-draw) (x (slot-value dialog-hud 'window-size)))
                               ;; current line-ending has hit the end of the allowed width
                               (setf line-ending (max (+ current-line-beginning 1)
                                                      ;; ^^ unlikely, but just in case a single word exceeds boundary
                                                      previous-word))
                               (return line-ending)))
                         :finally
                         ;; end of the string
                           (return line-ending)))
                    (get-or-create-line (line-number content)
                      (with-slots (lines) dialog-hud
                        (when (>= line-number (length lines) )
                          (let ((new-line (make-instance 'font-drawable
                                                         :width 100
                                                         :height 100
                                                         :parent dialog-hud
                                                         :text content)))
                            (when *gl-context*
                              (load-resources new-line *gl-context*))
                            (vector-push-extend new-line lines)))
                        (let ((line (elt lines line-number)))
                          (with-slots (window-position window-size window-padding background font-size)
                              dialog-hud
                            (let ((text-x (+ (x window-position) window-padding))
                                  (text-y (+ (y window-position) window-padding)))
                              (setf (text line) content
                                    (font-size line) (slot-value dialog-hud 'font-size)
                                    (color line) (slot-value dialog-hud 'color)
                                    (x line) text-x
                                    (y line) (+ text-y
                                                (* (height line) line-number)
                                                ;; TODO: slot value for line spacing
                                                (* line-number 1))
                                    ;; hack: force reload of font vbo
                                    (font-size line) 1
                                    (font-size line) (slot-value dialog-hud 'font-size))))
                          line))))
             (declare (inline compute-current-line-ending get-or-create-line space-p move-index-to-after-current-word move-index-to-start-of-next-word))
             (loop :with current-line = 0
                :and current-line-beginning = 0
                :and current-line-ending = -1
                :while (< current-line-ending (- (length text) 1)) :do
                  (with-slots (speaker-name background font-size) dialog-hud
                    ;; create speaker name
                    (when (and (= 0 current-line) speaker-name)
                      (let ((speaker-line (get-or-create-line current-line (format nil "~A:" speaker-name))))
                        (setf (font-size speaker-line) (- font-size 2)))
                      (incf current-line)))
                  (setf current-line-ending
                        (compute-current-line-ending current-line-beginning dialog-hud))
                  (get-or-create-line current-line
                                      (subseq text current-line-beginning current-line-ending))
                  (setf current-line-beginning (move-index-to-start-of-next-word current-line-ending))
                  (incf current-line)
                :finally
                  (with-slots (lines) dialog-hud
                    (loop :for i :from current-line :below (length lines) :do
                         (setf (parent (elt lines i)) nil)
                         (release-resources (elt lines i))
                       :finally (setf (fill-pointer lines) current-line)))))
        (release-resources font-draw)))))

@export
(defmethod show-dialog ((dialog-hud dialog-hud) text &key initiator speaker-name)
  (with-slots (show-p (hud-initiator initiator) (hud-speaker-name speaker-name) advance-delay) dialog-hud
    (when show-p
      (error "dialog already shown"))
    (let ((hud-input-id (if initiator
                             (active-input-device initiator)
                             *all-input-id*)))
      (when initiator
        (setf (active-input-device initiator) *no-input-id*))
      (schedule *scene*
                (+ (scene-ticks *scene*) advance-delay)
                (lambda ()
                  (setf (active-input-device dialog-hud) hud-input-id))))
    (setf hud-initiator initiator
          hud-speaker-name speaker-name
          (slot-value dialog-hud 'text) text
          show-p t)
    (%set-dialog-lines dialog-hud)))

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
