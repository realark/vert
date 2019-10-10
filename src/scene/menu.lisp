(in-package :recurse.vert)

;;;; Menus

(export '(select-down-sfx select-up-sfx run-action-sfx initialized-sfx title-scale item-scale menu-alignment))
(defclass menu (scene input-handler)
  ((node :initarg :root
         :initform (error ":root must be specified")
         :accessor node
         :documentation "The menu node currently being displayed.")
   (item-color :initarg :item-color
               :initform (make-color-rgba :b 255))
   (selected-item-color :initarg :selected-item-color
                        :initform (make-color-rgba :r 255 :g 215))
   (title-color :initarg :title-color
                :initform (make-color-rgba :r 255 :g 255 :b 255))
   (title-font-size
    :initarg :title-font-size
    :initform 48)
   (item-font-size
    :initarg :item-font-size
    :initform 36)
   (camera :initarg :camera
           :accessor camera
           :initform (make-instance 'camera
                                    :width (first (getconfig 'game-resolution *config*))
                                    :height (second (getconfig 'game-resolution *config*))))
   ;; TODO play music
   (music :initarg :music
          :initform nil
          :documentation "Music to play while the menu is active")
   (select-down-sfx :initarg :select-down-sfx
                    :initform nil
                    :documentation "Sound effect to play when selecting down")
   (select-up-sfx :initarg :select-up-sfx
                  :initform nil
                  :documentation "Sound effect to play when selecting up")
   (run-action-sfx :initarg :run-aciton-sfx
                   :initform nil
                   :documentation "Sound effect to play when selecting an action")
   (initialized-p :initform nil)
   (initialized-sfx :initarg :initialized-sfx
                    :initform nil
                    :documentation "Sound effect to play when menu is initialized.")
   (background :initarg :background
               :initform nil)
   (title-scale :initarg :title-scale
                :initform nil)
   (item-scale :initarg :item-scale
               :initform nil)
   (selection-marker :initform
                     (make-instance 'font-drawable
                                    :text ">")))
  (:documentation "A game menu"))

(defmethod initialize-instance :after ((menu menu) &rest args)
  (declare (ignore args))
  (when (node menu)
    (setf (node menu) (node menu)))
  (add-subscriber (camera menu) menu camera-screen-resized))

(defun %set-menu-position-and-color (menu)
  (with-slots (node
               camera
               selection-marker
               item-color selected-item-color
               title-color)
      menu
    (let ((current-y 5.0)
          (y-space-between-items (+ (height node) 1.0)))
      (multiple-value-bind (title-width title-height)
          (font-dimensions node)
        (setf title-width 100.0
              title-height 10.0)
        (setf (color node) title-color
              (width node) title-width
              (height node) title-height
              (x node) (+ (x camera)
                          (/ (width camera) 2.0)
                          (- (* (width node) 3/4)))
              (y node) current-y))
      (incf current-y (* 2 y-space-between-items))

      (setf (color selection-marker) *invisible*)
      (loop :for i :from 0 :below (length (slot-value node 'children)) :do
           (let* ((child (elt (slot-value node 'children) i))
                  (selected-p (= i (slot-value node 'selected-child-index)))
                  (child-color (if selected-p selected-item-color item-color)))
             (multiple-value-bind (item-width item-height)
                 (font-dimensions node)
               (setf item-width 100.0
                     item-height 10.0)
               (setf (color child) child-color
                     (width child) item-width
                     (height child) item-height
                     (x child) (+ (x camera)
                                  (/ (width camera) 2)
                                  (- (* (width child) 1/2)))
                     (y child) current-y)
               (when selected-p
                 (setf (color selection-marker) child-color
                       (width selection-marker) item-width
                       (height selection-marker) item-height
                       (x selection-marker) (- (x child) 10)
                       (y selection-marker) current-y)))
             (incf current-y y-space-between-items)))
      (let ((active-node (nth (slot-value node 'selected-child-index)
                              (slot-value node 'children))))
        (cond
          ;; move the camera up or down if all options are not visible
          ;; this code assumes all nodes are the same height and evenly spaced
          ;; this code will also break on very small displays (< 400px high)
          ((> (y camera)
              (- (y active-node)
                 y-space-between-items
                 (height active-node)))
           ;; node above active not fully visible
           (unless (= (y camera) (max 0 (- (y camera) (* 2 (height active-node)))))
             (setf (y camera) (max 0 (- (y camera) (* 2 (height active-node)))))))
          ((<= (+ (y camera) (height camera))
               (+ (y active-node)
                  (height active-node)
                  y-space-between-items
                  (height active-node)))
           ;; node below active not fully visible
           (incf (y camera) (* 2 (height active-node)))))))))

(defevent-callback camera-screen-resized ((camera camera) (menu menu))
  (%set-menu-position-and-color menu))

(set-default-input-command-map
 menu
 ("sdl-keyboard" (:scancode-return :menu-activate)
                 (:scancode-up :menu-up)
                 (:scancode-k :menu-up)
                 (:scancode-down :menu-down)
                 (:scancode-f11 :fullscreen-toggle)
                 (:scancode-j :menu-down))
 ("controller" (:11 :menu-up)
               (:12 :menu-down)
               (:0 :menu-activate)
               (:6 :menu-activate)))

(set-default-command-action-map
 menu
 (:fullscreen-toggle (on-deactivate (toggle-fullscreen
                                     (application-window *engine-manager*))))
 (:menu-activate (on-deactivate (activate menu (node menu) device-id)))
 (:menu-up (on-deactivate (select-up (node menu))
                          (%set-menu-position-and-color menu)))
 (:menu-down (on-deactivate (select-down (node menu))
                            (%set-menu-position-and-color menu))))

(defmethod (setf node) :after (value (menu menu))
  (%set-menu-position-and-color menu))

(defclass menu-node (font-drawable obb)
  ((menu :initarg :menu :initform (error ":menu required"))
   (node-name :initarg :node-name
              :initform (error ":node-name must be specified")
              :documentation "Name of this menu node")
   (text :initform nil)))

(defmethod initialize-instance :after ((node menu-node) &rest args)
  (declare (ignore args))
  (setf (text node) (slot-value node 'node-name)))

(defclass parent-node (menu-node)
  ((children :initarg :children
             :initform (list)
             :documentation "Child nodes of this node.")
   (selected-child-index :initform 0)))

(defgeneric select-down (parent-node)
  (:documentation "Select the item below the current selection.")
  (:method ((parent-node parent-node))
    (with-slots (children selected-child-index menu) parent-node
      (unless (= selected-child-index (1- (length children)))
        (with-slots (select-up-sfx) menu
          (when (and select-up-sfx *audio*)
            (play-sound-effect *audio* select-up-sfx)))
        (incf selected-child-index)))))

(defgeneric select-up (parent-node)
  (:documentation "Select the item above the current selection.")
  (:method ((parent-node parent-node))
    (with-slots (selected-child-index menu) parent-node
      (unless (= selected-child-index 0)
        (with-slots (select-down-sfx) menu
          (when (and select-down-sfx *audio*)
            (play-sound-effect *audio* select-down-sfx)))
        (decf selected-child-index)))))

(defclass action-node (menu-node)
  ((action :initarg :action
           :initform (error ":action must be specified")
           :documentation "No-arg closure to invoke when this node is selected")))

(defgeneric activate (menu node &optional device-id)
  (:documentation "Run NODE's action. DEVICE-ID specifies the input-id used to activate the menu.")
  (:method ((menu menu) (parent-node parent-node) &optional device-id)
    (if (eq (slot-value menu 'node) parent-node)
        (with-slots (children selected-child-index) parent-node
          (activate menu (elt children selected-child-index) device-id))
        (setf (slot-value menu 'node) parent-node)))
  (:method ((menu menu) (action-node action-node) &optional device-id)
    (with-slots (run-action-sfx) menu
      (when (and run-action-sfx *audio*)
        (play-sound-effect *audio* run-action-sfx)))
    (funcall (slot-value action-node 'action) device-id)))

(defmethod update ((menu menu) (delta-t-ms real) (null null))
  (pre-update menu)
  (pre-update (camera menu))
  (with-slots (initialized-p initialized-sfx) menu
    (unless (or (null initialized-sfx) initialized-p (null *audio*))
      (setf initialized-p t)
      (play-sound-effect *audio* initialized-sfx)))
  ;; update input
  (update menu delta-t-ms menu)
  (update (camera menu) delta-t-ms menu))

;; Render Menu

(defmethod render ((menu menu) update-percent camera renderer)
  (with-slots (node background) menu
    (let ((camera (camera menu)))
      (when background
        (render background 0.0 camera renderer))
      (render node 0.0 camera renderer)
      (loop for child in (slot-value node 'children) do
           (render child 0.0 camera renderer))
      (render (slot-value menu 'selection-marker) update-percent camera renderer))))

(defmethod load-resources :before ((node parent-node) renderer)
  (loop for child in (slot-value node 'children) do
       (load-resources child renderer)))

(defmethod release-resources :before ((node parent-node))
  (loop for child in (slot-value node 'children) do
       (release-resources child)))

(defmethod load-resources ((menu menu) renderer)
  (load-resources (slot-value menu 'selection-marker) renderer)
  (when (slot-value menu 'background)
    (load-resources (slot-value menu 'background) renderer))
  (load-resources (slot-value menu 'node) renderer)
  (%set-menu-position-and-color menu))

(defmethod release-resources ((menu menu))
  (release-resources (slot-value menu 'selection-marker))
  (when (slot-value menu 'background)
    (release-resources (slot-value menu 'background)))
  (release-resources (slot-value menu 'node)))

;; Menu Builder DSL

(defun %menu-list-to-tree (menu menu-list)
  "Convert a menu list into a menu tree."
  (let ((menu-name (first menu-list))
        (options (rest menu-list)))
    (assert (and (stringp menu-name)
                 (> (length options) 0)
                 (every #'listp options)))
    (make-instance 'parent-node
                   :scale (slot-value menu 'title-scale)
                   :menu menu
                   :width 1
                   :height 1
                   :node-name menu-name
                   :children
                   (loop with children = (list)
                      for option in options do
                        (push
                         (if (and (= (length option) 2) (functionp (second option)))
                             (make-instance 'action-node
                                            :scale (slot-value menu 'item-scale)
                                            :menu menu
                                            :width 1
                                            :height 1
                                            :node-name (first option)
                                            :action (second option))
                             (progn
                               (assert (>= (length option) 2))
                               (%menu-list-to-tree menu option)))
                         children)
                      finally (return (reverse children))))))

(defmacro %menu-dsl-to-list (menu-title &rest menu-options)
  "Eval MENU-TITLE and MENU-OPTIONS into a list which can be used by %MENU-LIST-TO-TREE"
  (unless (every #'listp menu-options)
    (error "Menu options must be lists."))
  `(list (let ((menu-title ,menu-title))
           ;; menu-title is never exposed to user code so no gensym needed.
           (if (stringp menu-title)
               menu-title
               (error "Expected string. Got ~A" menu-title)))
         ,@(loop for option in menu-options collect
                (progn
                  (unless (and (listp option) (>= (length option) 2))
                    (error "Option must be a list of at least two args: ~A." option))
                  (if (and (= 2 (length option))
                           (equalp "RUN-ACTION" (symbol-name (first (second option)))))
                      `(list (let ((option-title ,(first option)))
                               ;; option-title is never exposed to user code so no gensym needed.
                               (if (stringp option-title)
                                   option-title
                                   (error "Expected string. Got ~A" option-title)))
                             (lambda (device-id)
                               (declare (ignorable device-id))
                               ,@(rest (second option))))
                      `(%menu-dsl-to-list ,@option))))))

;; TODO: Document DEVICE-ID
(defmacro create-menu ((menu-class &rest menu-options) &body menu-dsl)
  "Create a MENU instance with the given MENU-OPTIONS and menu tree built from MENU-TREE.

   Grammar of MENU-DSL:
   menu-dsl    := (menu-name option+)
   menu-name   := string
   option      := (option-name run-action) | menu-dsl
   option-name := string
   run-action  := (run-action lisp-form*)"
  (alexandria:with-gensyms (menu)
    `(let ((,menu (make-instance ',menu-class
                                 ,@menu-options
                                 :root nil)))
       (setf (slot-value ,menu 'node)
             (%menu-list-to-tree ,menu (%menu-dsl-to-list ,@menu-dsl)))
       ,menu)))
