(in-package :recurse.vert/unit-test)

(deftest menu-list-to-tree
  "Test list -> tree conversion"
  (let ((menu-tree (recurse.vert::%menu-list-to-tree
                    (list
                     "Root"
                     (list "Option 1" (lambda () "do option 1"))
                     (list "Option 2"
                           (list "2a" (lambda () '2a))
                           (list "2b"
                                 (list "2b.I" (lambda () '2bI))
                                 (list "2b.II" (lambda () '2bII)))
                           (list "2c" (lambda () '2b)))))))
    (with-slots ((node-name recurse.vert::node-name) (children recurse.vert::children)) menu-tree
      (prove:is node-name "Root" :test #'equalp)
      (prove:is (length children) 2 "2 menu options" :test #'=)
      (prove:is (typep (first children) 'recurse.vert::action-node) T "First option is action node.")
      (with-slots ((node-name recurse.vert::node-name) (action recurse.vert::action)) (first children)
        (prove:is node-name "Option 1" :test #'equalp)
        (prove:is (funcall action) "do option 1" :test #'equalp))

      (prove:is (typep (second children) 'recurse.vert::parent-node) T "Second option is submenu.")
      (with-slots ((node-name recurse.vert::node-name) (subchildren recurse.vert::children)) (second children)
        (prove:is (length subchildren) 3 "Three subchildren" :test #'=)
        (prove:is (typep (first subchildren) 'recurse.vert::action-node) T "2a is action")
        (prove:is (typep (second subchildren) 'recurse.vert::parent-node) T "2b is submenu")
        (prove:is (typep (third subchildren) 'recurse.vert::action-node) T "2c is action")))))

(deftest menu-dsl-to-list
  "Test menu body -> list conversion"
  ;; handle bad syntax
  (prove:is-error (eval '(recurse.vert::%menu-dsl-to-list "No-Option-Menu" ()))
                  error
                  "Menus must have at least one option.")
  (prove:is-error (eval '(recurse.vert::%menu-dsl-to-list "Menu" ("Unquoted Option" (progn))))
                  error
                  "Menu action form must be quoted.")
  (prove:is-error (eval '(recurse.vert::%menu-dsl-to-list "Menu" ("Too many args" (run-action (progn 1)) (run-action (progn 2)))))
                  error
                  "Only one quoted action form.")
  (prove:is-error (eval '(recurse.vert::%menu-dsl-to-list "Menu" ("Submenu" 'a 'b 'c)))
                  error
                  "(Sub)Menu options must be lists")
  (prove:is-error (eval '(recurse.vert::%menu-dsl-to-list 'Menu ("Option" (run-action (progn 'op1)))))
                  error
                  "Menu name must be a string")
  (prove:is-error (eval '(recurse.vert::%menu-dsl-to-list "Menu" ('Option (run-action (progn 'op1)))))
                  error
                  "Menu option must be a string")

  (let ((menu-list (recurse.vert::%menu-dsl-to-list "Menu"
                                                    ("Option1" (run-action 'op1))
                                                    ("Submenu"
                                                     ("Sub1" (run-action 'sub1))
                                                     ("Sub2" (run-action 'sub2)))
                                                    ("Option3" (run-action 'op3)))))
    (prove:is (first menu-list) "Menu")
    (prove:is (length menu-list) 4 "Top level menu correct size")
    (prove:is (length (second menu-list)) 2 "First option correct size")
    (prove:is (funcall (second (second menu-list)) nil) 'op1 "First option correct function")
    (prove:is (funcall (second (third (third menu-list))) nil) 'sub2 "Submenu 2 correct function")
    (prove:is (funcall (second (fourth menu-list)) nil) 'op3 "Third option correct function")))

(deftest menu-navigation
  (let* (sub-1-activated
         sub-2-activated
         (menu (create-menu (menu)
                 "Test"
                 ("Option1" (run-action 'Option1))
                 ("Option2" ("Sub1" (run-action (setf sub-1-activated T)))
                            ("Sub2" (run-action (setf sub-2-activated T))))
                 ("Option3" (run-action 'Option3)))))
    (flet ((is-menu (expected-node-name &optional docstring)
             "Assert that the menu's name matches EXPECTED-NODE-NAME"
             (prove:is (slot-value (slot-value menu 'recurse.vert::node) 'recurse.vert::node-name)
                       expected-node-name
                       docstring
                       :test #'equalp))
           (is-selected (expected-node-name &optional docstring)
             "Assert that the menu's selected child name matches EXPECTED-NODE-NAME"
             (with-slots ((children recurse.vert::children)
                          (selected-child-index recurse.vert::selected-child-index))
                 (slot-value menu 'recurse.vert::node)
               (prove:is (slot-value (elt children selected-child-index) 'recurse.vert::node-name)
                         expected-node-name
                         docstring
                         :test #'equalp))))
      (is-menu "Test" "Top Level Menu Selected")
      (is-selected "Option1" "Option1 initially selected")
      (recurse.vert::select-up (slot-value menu 'recurse.vert::node))
      (is-selected "Option1" "Can't move above option1")
      (recurse.vert::select-down (slot-value menu 'recurse.vert::node))
      (is-selected "Option2" "Move To Option 2")
      (recurse.vert::select-down (slot-value menu 'recurse.vert::node))
      (is-selected "Option3" "Move To Option 3")
      (recurse.vert::select-down (slot-value menu 'recurse.vert::node))
      (is-selected "Option3" "Can't move below option3")

      (recurse.vert::select-up (slot-value menu 'recurse.vert::node))
      (recurse.vert::activate menu (slot-value menu 'recurse.vert::node))
      (is-menu "Option2" "Sub Level Menu Selected")
      (is-selected "Sub1" "Sub1 initially selected")
      (recurse.vert::select-up (slot-value menu 'recurse.vert::node))
      (is-selected "Sub1" "Can't move above sub1")
      (recurse.vert::activate menu (slot-value menu 'recurse.vert::node))
      (prove:is sub-1-activated T "Sub1 Activated")
      (recurse.vert::select-down (slot-value menu 'recurse.vert::node))
      (is-selected "Sub2" "Select sub2")
      (recurse.vert::select-down (slot-value menu 'recurse.vert::node))
      (is-selected "Sub2" "Can't move below sub2")
      (recurse.vert::activate menu (slot-value menu 'recurse.vert::node))
      (prove:is sub-2-activated T "Sub2 Activated"))))
