(in-package :recurse.vert)

;;;; Menus

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
                                    :width 320
                                    :height 180))
   ;; TODO play music
   (music :initarg :music
          :initform nil
          :documentation "Music to play while the menu is active")
   (background :initarg :background
               :initform nil))
  (:documentation "A game menu"))

(defmethod initialize-instance :after ((menu menu) &rest args)
  (declare (ignore args))
  (setf (node menu) (node menu))
  (add-subscriber (camera menu) menu camera-screen-resized))

(defun %set-menu-position-and-color (menu)
  (with-slots (node
               camera
               item-color selected-item-color
               title-color)
      menu
    (let ((current-y 5.0)
          (y-space-between-items (+ (height node) 5.0)))
      (multiple-value-bind (title-width title-height)
          (font-dimensions node)
        (setf title-width 100.0
              title-height 10.0)
        (setf (color node) title-color
              (width node) title-width
              (height node) title-height
              (x node) (+ (x camera)
                          (/ (width camera) 2.0)
                          (- (/ (width node) 2.0)))
              (y node) current-y))
      (incf current-y y-space-between-items)

      (loop for i from 0 below (length (slot-value node 'children)) do
           (let ((child (elt (slot-value node 'children) i))
                 (child-color (if (= i (slot-value node 'selected-child-index))
                                  selected-item-color
                                  item-color)))
             (multiple-value-bind (item-width item-height)
                 (font-dimensions node)
               (setf item-width 100.0
                     item-height 10.0)
               (setf (color child) child-color
                     (width child) item-width
                     (height child) item-height
                     (x child) (+ (x camera)
                                  (/ (width camera) 2)
                                  (- (/ (width child) 2)))
                     (y child) current-y))
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
  ((node-name :initarg :node-name
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
    (with-slots (children selected-child-index) parent-node
      (unless (= selected-child-index (1- (length children)))
        (incf selected-child-index)))))

(defgeneric select-up (parent-node)
  (:documentation "Select the item above the current selection.")
  (:method ((parent-node parent-node))
    (with-slots (selected-child-index) parent-node
      (unless (= selected-child-index 0)
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
    (funcall (slot-value action-node 'action) device-id)))

(defmethod update ((menu menu) (delta-t-ms real) (null null))
  (update (camera menu) delta-t-ms menu)
  ;; update input
  (update menu delta-t-ms menu))

;; Render Menu

(defmethod render ((menu menu) update-percent camera renderer)
  (with-slots (node background) menu
    (let ((camera (camera menu)))
      (when background
        (render background 0.0 camera renderer))
      (render node 0.0 camera renderer)
      (loop for child in (slot-value node 'children) do
           (render child 0.0 camera renderer)))))

(defmethod load-resources :before ((node parent-node) renderer)
  (loop for child in (slot-value node 'children) do
       (load-resources child renderer)))

(defmethod release-resources :before ((node parent-node))
  (loop for child in (slot-value node 'children) do
       (release-resources child)))

(defmethod load-resources ((menu menu) renderer)
  (when (slot-value menu 'background)
    (load-resources (slot-value menu 'background) renderer))
  (load-resources (slot-value menu 'node) renderer)
  (%set-menu-position-and-color menu))

(defmethod release-resources ((menu menu))
  (when (slot-value menu 'background)
    (release-resources (slot-value menu 'background)))
  (release-resources (slot-value menu 'node)))

;; Menu Builder DSL

(defun %menu-list-to-tree (menu-list)
  "Convert a menu list into a menu tree."
  (let ((menu-name (first menu-list))
        (options (rest menu-list)))
    (assert (and (stringp menu-name)
                 (> (length options) 0)
                 (every #'listp options)))
    (make-instance 'parent-node
                   :width 1
                   :height 1
                   :node-name menu-name
                   :children
                   (loop with children = (list)
                      for option in options do
                        (push
                         (if (and (= (length option) 2) (functionp (second option)))
                             (make-instance 'action-node
                                            :width 1
                                            :height 1
                                            :node-name (first option)
                                            :action (second option))
                             (progn
                               (assert (>= (length option) 2))
                               (%menu-list-to-tree option)))
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
  `(make-instance ',menu-class
                  ,@menu-options
                  :root (%menu-list-to-tree (%menu-dsl-to-list ,@menu-dsl))))
