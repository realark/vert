(in-package :recurse.vert)

@export
(defclass dialog-speaker (game-object)
  ((object-name :initarg :object-name
                :initform (error ":object-name required")
                :reader object-name
                :documentation "Override of GAME-OBJECT's name slot to force setting the name."))
  (:documentation "A game object which my product dialog."))

@export
(defclass dialog-hud (overlay input-handler)
  ((show-p :initform nil)
   (initiator :initform nil
              :accessor dialog-hud-initiator
              :documentation "game-object to return input control to once this HUD closes.")
   (window-position :initarg :window-position
                    :initform nil)
   (window-size :initarg :window-size
                :initform nil)
   (window-padding :initarg :window-padding
                   :initform 1
                   :documentation "Guaranteed amount of space on all sides of the window which will not contain text.")
   (speaker :initform nil
            :accessor dialog-hud-speaker)
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
                  :initform 0
                  :documentation "time (ms) before player is allowed to advance the dialog.")))
(export '(dialog-hud-initiator dialog-hud-speaker))

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

@export
(defmethod advance-dialog ((dialog-hud dialog-hud))
  (quit-dialog dialog-hud))

@export
(defmethod quit-dialog (dialog-hud)
  (declare (dialog-hud dialog-hud))
  (with-slots (show-p initiator speaker) dialog-hud
    (when initiator
      (setf (active-input-device initiator) (active-input-device dialog-hud)
            initiator nil))
    (setf (active-input-device dialog-hud) *no-input-id*
          speaker nil
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
                  (with-slots (speaker background font-size) dialog-hud
                    ;; create speaker name
                    (when (and (= 0 current-line) speaker)
                      (let ((speaker-line (get-or-create-line current-line (format nil "~A:" (object-name speaker)))))
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

(defmethod (setf dialog-hud-initiator) :before (new-initiator (hud dialog-hud))
  (with-slots ((current-initiator initiator) advance-delay) hud
    (when current-initiator
      (error "initiator already set: ~A" current-initiator))
    (let ((hud-input-id (if new-initiator
                            (active-input-device new-initiator)
                            *all-input-id*)))
      (when new-initiator
        (setf (active-input-device new-initiator) *no-input-id*))
      (schedule *scene*
                (+ (scene-ticks *scene*) advance-delay)
                (lambda ()
                  (setf (active-input-device hud) hud-input-id))))))

@export
(defmethod show-dialog ((dialog-hud dialog-hud) text &key initiator speaker)
  (with-slots (show-p advance-delay) dialog-hud
    (when (not (dialog-hud-initiator dialog-hud))
      (setf (dialog-hud-initiator dialog-hud) initiator))
    (when (not (dialog-hud-speaker dialog-hud))
      (setf (dialog-hud-speaker dialog-hud) speaker))
    (unless show-p
      (setf show-p t))
    (setf (slot-value dialog-hud 'text) text)
    (%set-dialog-lines dialog-hud)))

(set-default-input-command-map
 dialog-hud
 ("controller"
  (:0 :advance-dialog)
  (:2 :advance-dialog))
 ("sdl-keyboard"
  (:scancode-space :advance-dialog)
  (:scancode-t :advance-dialog)))

(set-default-command-action-map
 dialog-hud
 (:advance-dialog
  (on-activate (advance-dialog dialog-hud))))

;;;; cutscene node base class

@export
(defclass cutscene-node (game-object)
  ((next-nodes :initarg :next-nodes
               :initform (list)
               :accessor cutscene-node-next-nodes
               :documentation "list of the next nodes this node may jump to. If nil, the next node will terminate the cutscene")
   (next-node-index :initarg :next-node-index
                    :initform nil
                    :accessor cutscene-node-next-node-index
                    :documentation "index pointing to some value in NEXT-NODES list.
If nil, this node will block the cutscene. If non-nil, this node will be deactivated and the next node activated."))
  (:documentation "A single action performed in a custscene. Could change the dialog, move a character, etc."))
(export '(cutscene-node-next-nodes cutscene-node-next-node-index))

;;;; HUD to play cutscenes

@export
(defclass cutscene-hud (dialog-hud)
  ((active-node :initarg :active-node
                :initform nil)
   (on-quit-callback :initform nil
                     :documentation "zero-arg function to run when the cutscene finishes."))
  (:documentation "A more advanced dialog hud which may run arbitrary actions along with presenting text"))

(defmethod update ((hud cutscene-hud) timestep scene)
  (with-slots (active-node) hud
    (when active-node
      (cutscene-node-while-active active-node hud)
      (let ((next-node (cutscene-node-next-node active-node)))
        (play-cutscene hud next-node))))
  (call-next-method hud timestep scene))

@export
(defmethod play-cutscene ((hud cutscene-hud) new-node &key on-quit)
  "Play a cutscene starting with NEW-NODE. ON-QUIT is an optional zero-arg fn to invoke when the cutscene finishes."
  (with-slots ((current-node active-node) on-quit-callback) hud
    (unless (eq new-node current-node)
      (when current-node
        (cutscene-node-on-deactivate current-node hud))
      (when new-node
        (cutscene-node-on-activate new-node hud))
      (setf current-node new-node)
      (when on-quit
        (when on-quit-callback
          (log:error "cutscene clobbering existing callback: old: ~A new:~A" on-quit-callback on-quit))
        (setf on-quit-callback on-quit))
      (when (null current-node)
        (quit-dialog hud)
        (let ((callback on-quit-callback))
          (setf on-quit-callback nil)
          (when callback
            (locally (declare ((function ()) callback))
              (funcall callback))))))))

(defmethod advance-dialog ((hud cutscene-hud))
    (with-slots (active-node on-quit-callback) hud
      (if active-node
          (advance-cutscene-node active-node hud)
          (call-next-method hud))))

;;;; base logic and cutscene-node hooks

(defmethod cutscene-node-next-node ((node cutscene-node))
  "Get the next node to play in the cutscene.
May be the same CUTSCENE-NODE to continue the same action, or nil to quit the cutscene."
  (with-slots (next-nodes next-node-index) node
    (if next-node-index
        (when next-nodes ; note: returning nil (i.e. quit cutscene) if no next nodes to jump to
          (elt next-nodes next-node-index ))
        ;; no index set. Continue to run with this node
        node)))

(defgeneric cutscene-node-select-right (node)
  (:method ((node cutscene-node))))

(defgeneric cutscene-node-select-left (node)
  (:method ((node cutscene-node))))

@export
(defmethod advance-cutscene-node ((node cutscene-node) (hud cutscene-hud))
  "Invoked on the active CUTSCENE-NODE when the player advances the cutscene (i.e. presses a button)"
  ;; no-op
  )

@export
(defmethod cutscene-node-on-activate ((node cutscene-node) (hud cutscene-hud)))

@export
(defmethod cutscene-node-on-deactivate ((node cutscene-node) (hud cutscene-hud))
  (setf (cutscene-node-next-node-index node) nil))

@export
(defmethod cutscene-node-while-active ((node cutscene-node) (hud cutscene-hud))
  (setf (cutscene-node-next-node-index node) 0))

;;;; Macro to build cutscene tree

@export
(defmacro make-cutscene (() &body body)
  "Macro to to create a dialog tree. Each form in body may either be a function which returns a CUTSCENE-NODE, or a string (which will be turned into a SHOW-DIALOG).
The NEXT value of each node defaults to the next line in BODY. "
  (alexandria:with-gensyms (id-hash node nodes i next-node-id)
    `(let* ((,id-hash (make-hash-table :test #'equal))
            (,nodes (list ,@(loop :for node-form :in body
                               :collect `(let ((,node ,(cond ((typep node-form 'string)
                                                              `(cutscene-show-dialog ,node-form))
                                                             ((and (listp node-form) (> (length node-form) 0))
                                                              node-form)
                                                             (t (error "illegal cutscene option: ~A. Must be a function which evals to a cutscene or a raw string."
                                                                       node-form)))))
                                           (when (gethash (object-id ,node) ,id-hash)
                                             (error "Cutscene node with id ~A defined more than once. First: ~A, Second: ~A"
                                                    (object-id ,node)
                                                    (gethash (object-id ,node) ,id-hash)
                                                    ,node))
                                           (setf (gethash (object-id ,node) ,id-hash) ,node)
                                           ,node)))))
       (loop :for ,i :from 0 :below (length ,nodes) :do
            (let ((,node (elt ,nodes ,i)))
              (if (cutscene-node-next-nodes ,node)
                  (setf (cutscene-node-next-nodes ,node)
                        (mapcar (lambda (,next-node-id)
                                  (or (gethash ,next-node-id ,id-hash)
                                      (error "~A's next value not found in cutscene: ~A"
                                             ,node
                                             ,next-node-id)))
                                (cutscene-node-next-nodes ,node)))
                  (when (< ,i (- (length ,nodes) 1))
                    ;; if node does not define a next node, set to the node which appears on the next line
                    (setf (cutscene-node-next-nodes ,node)
                          (list (elt ,nodes (+ ,i 1))))))))
       ;; return the root node
       (first ,nodes))))

#+nil
(make-cutscene ()
  (change-speaker 'someone :object-id 'welcome)
  (move-camera :x (x *player*) :y (y *player*))
  (show-text "Hello, Human. Welcome to the game")
  "You can also show text like this."

  (label 'ask-question)
  (ask-question "I'm a spirit guardian of a long forgotten empire. Would you like to learn the history of my people?"
                ("Yes" (go-to-node 'answer-yes))
                ("No" (go-to-node 'answer-no))
                ("Maybe...?" "I'll say that again."
                             (go-to-node 'ask-question)))
  (label 'answer-no)
  "Oh... I'll just give you the mythical sword of the stone-king then. You won't **really** appreciate it though."
  (go-to-node 'after-tale)
  (label 'answer-yes)
  "Great! I will tell you a grand tale..."
  (label 'after-tale)
  (run-action (log "This is arbitrary lisp code")
              (give-to-player *stone-sword*)
              (play-sound-effect *audio* *recieve-special-item*))
  "Good luck on your journey."

  (run-action (format t "debugging"))
  (pause-ms 500))

;;;; built-in cutscene nodes

;; show dialog
(defclass cutscene-node-show-dialog (cutscene-node)
  ((text :initarg :text :initform (error ":text required"))
   (wait-for-input-p :initarg :wait-for-input-p :initform t)))

(defmethod cutscene-node-on-activate ((node cutscene-node-show-dialog) (hud cutscene-hud))
  (show-dialog hud (slot-value node 'text)))

(defmethod cutscene-node-while-active ((node cutscene-node-show-dialog) (hud cutscene-hud))
  (with-slots (wait-for-input-p next-node-index) node
    (when (not wait-for-input-p)
      (setf next-node-index 0))))

;; only advance dialog nodes when player presses input
(defmethod advance-cutscene-node ((node cutscene-node-show-dialog) (hud cutscene-hud))
  ;; set next-node-index to the appropriate index
  (with-slots (next-node-index) node
    (setf next-node-index 0)))

@export
(defun cutscene-show-dialog (text &key (wait-for-input-p t))
  "Show dialog. When WAIT-FOR-INPUT-P is t, wait for the player to advance the cutscene."
  (make-instance 'cutscene-node-show-dialog
                 :text text
                 :wait-for-input-p wait-for-input-p))

;; waiting / sleeping
(defclass cutscene-node-wait (cutscene-node)
  ((wait-time :initarg :wait-time :initform (error ":wait-time required"))
   (t0 :initform 0)))

(defmethod cutscene-node-on-activate ((node cutscene-node-wait) (hud cutscene-hud))
  (with-slots (t0) node
    (setf t0 (scene-ticks *scene*))))

;; do nothing while text is active (await player input)
(defmethod cutscene-node-while-active ((node cutscene-node-wait) (hud cutscene-hud))
    (with-slots (t0 wait-time) node
      (let ((now (scene-ticks *scene*)))
        (when (>= now (+ t0 wait-time))
          (setf (cutscene-node-next-node-index node) 0)))))

@export
(defun cutscene-wait (ms)
  (make-instance 'cutscene-node-wait :wait-time ms))

;; arbitrary action
(defclass cutscene-node-run-action (cutscene-node)
  ((zero-arg-fn :initarg :zero-arg-fn
                :initform (error ":zero-arg-fn required")))
  (:documentation "A cutscene node which runs a zero-arg fn. Cutscene advances when the function returns non-nil"))

@export
(defun cutscene-run-action (zero-arg-fn)
  (make-instance 'cutscene-node-run-action :zero-arg-fn zero-arg-fn))

(defmethod cutscene-node-while-active ((node cutscene-node-run-action) (hud cutscene-hud))
  (with-slots (zero-arg-fn) node
    (declare ((function ()) zero-arg-fn))
    (when (funcall zero-arg-fn)
      (setf (cutscene-node-next-node-index node) 0))))

;; specific actions on hud
(defclass cutscene-node-run-action-with-hud (cutscene-node)
  ((one-arg-fn :initarg :one-arg-fn
               :initform (error ":one-arg-fn required")))
  (:documentation "A cutscene node which runs one-arg fn with the cutscene HUD passed."))

(defmethod cutscene-node-while-active ((node cutscene-node-run-action-with-hud) (hud cutscene-hud))
  (with-slots (one-arg-fn) node
    (declare ((function (cutscene-hud)) one-arg-fn))
    (when (funcall one-arg-fn hud)
      (setf (cutscene-node-next-node-index node) 0))))

(defun cutscene-run-action-with-hud (one-arg-fn)
  (make-instance 'cutscene-node-run-action-with-hud :one-arg-fn one-arg-fn))

@export
(defun cutscene-change-speaker (new-speaker)
  (cutscene-run-action-with-hud
   (lambda (hud)
     (setf (dialog-hud-speaker hud) new-speaker))))

@export
(defun cutscene-capture-input (initiator)
  "Stop INITIATOR's input and return it once the cutscene is over."
  (cutscene-run-action-with-hud
   (lambda (hud)
     (setf (dialog-hud-initiator hud) initiator)
     t)))

;; hid dialog -hud
@export
(defun cutscene-hide-dialog-hud ()
  (cutscene-run-action-with-hud
   (lambda (hud)
     (setf (slot-value hud 'show-p) nil)
     t)))

;; labels and gotos

@export
(defun cutscene-label (label-name)
  (make-instance 'cutscene-node
                 :object-id label-name))

@export
(defun cutscene-goto (node-id)
  (make-instance 'cutscene-node
                 :next-nodes (list node-id)
                 :next-node-index 0))

;; dialog input
(defclass cutscene-node-ask-question (cutscene-node-show-dialog)
  ((answers :initarg :answers
            :initform (error "answers required")
            :documentation "list of human-readable answer strings")
   (answer-padding :initarg :answer-padding :initform 100)
   (answer-indicator :initarg :answer-indicator :initform nil)
   (select-sfx :initarg :select-sfx :initform nil)
   (selection-made-sfx :initarg :selection-made-sfx :initform nil)
   (selected-answer-index :initform 0)))

(defmethod cutscene-node-on-activate ((node cutscene-node-ask-question) (hud cutscene-hud))
  (call-next-method node hud)
  (with-slots ((padding answer-padding)
               (index selected-answer-index)
               (indicator answer-indicator)
               answers)
      node
    (when indicator
      ;; hack: setting here first so indicator renders behind answer
      (setf (parent indicator) hud))
    (multiple-value-bind (ans-w ans-h)
        (loop :with total-width = 0 :and total-height = 0
           :for answer in (slot-value node 'answers) :do
             (incf total-width (+ (width answer) padding))
             (setf total-height (max total-height (height answer)))
           :finally
             (decf total-width padding) ; don't pad the last element
             (return (values total-width total-height)))
      (with-slots (window-position window-size) hud
        ;; center answers on the last line of the dialog window
        (loop :with x = (+ (x window-position) (max 0 (/ (- (width window-size) ans-w) 2.0)))
           ;; note: hardcoded 5.0 is a hack to make things look nice
           :with y = (- (+ (y window-position) (height window-size)) ans-h 5.0)
           :for answer in answers :do
             (setf (parent answer) hud
                   (x answer) x
                   (y answer) y)
             (incf x (+ (width answer) padding))))
      (setf index 0)
      (when indicator
        (setf (x indicator) (x (elt answers index))
              (y indicator) (y (elt answers index))
              (width indicator) (width (elt answers index))
              (height indicator) (+ (height (elt answers index))))))))

(defmethod cutscene-node-on-deactivate ((node cutscene-node-ask-question) (hud cutscene-hud))
  (with-slots (answers answer-indicator (sfx selection-made-sfx)) node
    (when answer-indicator
      (setf (parent answer-indicator) nil))
    (when sfx
      (play-sound-effect *audio* sfx))
    (loop :for answer in answers :do
         (setf (parent answer) nil)))
  (call-next-method node hud))

(defmethod advance-cutscene-node ((node cutscene-node-ask-question) (hud cutscene-hud))
  ;; answers and next-nodes lists correspond to each other
  (with-slots (answers (answer-index selected-answer-index) (node-index next-node-index)) node
    (setf node-index answer-index)))

(defmethod cutscene-node-select-right ((node cutscene-node-ask-question))
  (with-slots (answers
               (index selected-answer-index)
               (sfx select-sfx)
               (indicator answer-indicator))
      node
    (setf index (min (+ index 1) (- (length answers) 1)))
    (when sfx
      (play-sound-effect *audio* sfx))
    (when indicator
      (setf (x indicator) (x (elt answers index))
            (y indicator) (y (elt answers index))
            (width indicator) (width (elt answers index))
            (height indicator) (+ (height (elt answers index)))))))

(defmethod cutscene-node-select-left ((node cutscene-node-ask-question))
  (with-slots (answers
               (index selected-answer-index)
               (sfx select-sfx)
               (indicator answer-indicator))
      node
    (setf index (max (- index 1) 0))
    (when sfx
      (play-sound-effect *audio* sfx))
    (when indicator
      (setf (x indicator) (x (elt answers index))
            (y indicator) (y (elt answers index))
            (width indicator) (width (elt answers index))
            (height indicator) (+ (height (elt answers index)))))))

@export
(defun cutscene-ask (prompt &key answer-indicator select-sfx selection-made-sfx answers)
  (unless (>= (length answers) 2)
    (error "At least two answers required"))
  (loop :for answer :in answers :do
       (unless (and (consp answer)
                    (typep (car answer) 'game-object)
                    (symbolp (cdr answer)))
         (error "each answer must be a (cons game-object symbol)")))
  ;; show the prompt on screen
  (make-instance 'cutscene-node-ask-question
                 :answer-indicator answer-indicator
                 :select-sfx select-sfx
                 :selection-made-sfx selection-made-sfx
                 :text prompt
                 :next-nodes
                 (mapcar (lambda (answer)
                           (cdr answer))
                         answers)
                 :answers
                 (mapcar (lambda (answer)
                           (car answer))
                         answers)))


(set-default-input-command-map
 cutscene-hud
 ("controller"
  (:2 :advance-dialog)
  (:14 :select-right)
  (:13 :select-left))
 ("sdl-keyboard"
  (:scancode-t :advance-dialog)
  (:scancode-right :select-right)
  (:scancode-l :select-right)
  (:scancode-left :select-left)
  (:scancode-h :select-left)))

(set-default-command-action-map
 cutscene-hud
 (:advance-dialog
  (on-activate (advance-dialog cutscene-hud)))
 (:select-right
  (on-activate
   (with-slots (active-node) cutscene-hud
     (cutscene-node-select-right active-node))))
 (:select-left
  (on-activate
   (with-slots (active-node) cutscene-hud
     (cutscene-node-select-left active-node)))))
