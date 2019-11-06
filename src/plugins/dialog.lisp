(in-package :recurse.vert)

@export
(defclass dialog-speaker (game-object)
  ((name :initarg :name
         :initform (error ":name required")
         :reader name))
  (:documentation "A game object which my product dialog."))
(export '(name))

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
                  :initform 500
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
                      (let ((speaker-line (get-or-create-line current-line (format nil "~A:" (name speaker)))))
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
  (:2 :advance-dialog))
 ("sdl-keyboard"
  (:scancode-t :advance-dialog)))

(set-default-command-action-map
 dialog-hud
 (:advance-dialog
  (on-activate (advance-dialog dialog-hud))))

;;;; cutscene node base class

@export
(defclass cutscene-node (game-object)
  ((root-node :initarg :root-node
              :initform nil
              :documentation "Root node for cutscene graph. Nil iff this node is the root.")
   (next-nodes :initarg :next-nodes
               :initform (list)
               :accessor cutscene-node-next-nodes
               :documentation "list of the next nodes this node may jump to. If nil, the next node will terminate the cutscene")
   (next-node-index :initform nil
                    :accessor cutscene-node-next-node-index
                    :documentation "index pointing to some value in NEXT-NODES list.
If nil, this node will block the cutscene. If non-nil, this node will be deactivated and the next node activated."))
  (:documentation "A single action performed in a custscene. Could change the dialog, move a character, etc."))
(export '(cutscene-node-next-nodes cutscene-node-next-node-index))

;;;; HUD to play cutscenes

@export
(defclass cutscene-hud (dialog-hud)
  ((active-node :initarg :active-node
                :initform nil))
  (:documentation "A more advanced dialog hud which may run arbitrary actions along with presenting text"))

(defmethod update ((hud cutscene-hud) timestep scene)
  (with-slots (active-node) hud
    (when active-node
      (cutscene-node-while-active active-node hud)
      (let ((next-node (cutscene-node-next-node active-node)))
        (play-cutscene hud next-node))))
  (call-next-method hud timestep scene))

@export
(defmethod play-cutscene ((hud cutscene-hud) new-node)
  (with-slots ((current-node active-node)) hud
    (unless (eq new-node current-node)
      (when current-node
        (cutscene-node-on-deactivate current-node hud))
      (when new-node
        (cutscene-node-on-activate new-node hud))
      (setf current-node new-node)
      (when (null current-node)
        (quit-dialog hud)))))

(defmethod advance-dialog ((hud cutscene-hud))
    (with-slots (active-node) hud
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

@export
(defmethod advance-cutscene-node ((node cutscene-node) (hud cutscene-hud))
  "Invoked on the active CUTSCENE-NODE when the player advances the cutscene (i.e. presses a button)"
  ;; set next-node-index to the appropriate index
  (with-slots (next-node-index) node
    (setf next-node-index 0)))

@export
(defmethod cutscene-node-on-activate ((node cutscene-node) (hud cutscene-hud)))

@export
(defmethod cutscene-node-on-deactivate ((node cutscene-node) (hud cutscene-hud)))

@export
(defmethod cutscene-node-while-active ((node cutscene-node) (hud cutscene-hud))
  (setf (cutscene-node-next-node-index node) 0))

;;;; Macro to build cutscene tree

@export
(defmacro make-cutscene ((&key initiator (prefix 'cutscene)) &body body)
  "Macro to to create a dialog tree. Each form in body may either be a function which returns a CUTSCENE-NODE, or a string (which will be turned into a SHOW-DIALOG).
The NEXT value of each node defaults to the next line in BODY.
All functions will be resolved to <PREFIX><function-name>, or just <function-name> if the prefix'd symbol is not a function.
"
  (alexandria:with-gensyms (id-hash node nodes i next-node-id)
    `(let* ((,id-hash (make-hash-table :test #'equal))
           (,nodes (list ,@(loop :for node-form :in body
                              ;; add prefix to node creators where appropriate and convert raw strings
                              :collect `(let ((,node ,(cond ((typep node-form 'string)
                                                             `(cutscene-show-dialog ,node-form))
                                                            ((and (listp node-form) (> (length node-form) 0))
                                                             (let ((prefixed-name (alexandria:symbolicate prefix (first node-form))))
                                                               `(if (fboundp ,prefixed-name)
                                                                    ,(push prefixed-name (rest node-form))
                                                                    ,node-form)))
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
(make-cutscene (:initiator 'the-player)
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

(defclass cutscene-node-show-dialog (cutscene-node)
  ((text :initarg :text :initform (error ":text required"))))

(defmethod cutscene-node-on-activate ((node cutscene-node-show-dialog) (hud cutscene-hud))
  (show-dialog hud (slot-value node 'text)))

;; do nothing while text is active (await player input)
(defmethod cutscene-node-while-active ((node cutscene-node-show-dialog) (hud cutscene-hud)))

@export
(defun cutscene-show-dialog (text)
  (make-instance 'cutscene-node-show-dialog
                 :text text))

@export
(defun cutscene-change-speaker (new-speaker)
  (error "TODO"))

@export
(defun cutscene-quit ()
  "Terminate the cutscene."
  (error "TODO"))
