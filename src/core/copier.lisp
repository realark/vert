(in-package :recurse.vert)

(defgeneric %deep-copy (object &key copied-objects)
  ;; NOTE: some sbcl specific stuff here
  (:documentation "Makes and returns a deep copy of OBJECT")
  (:method ((null null) &key copied-objects) nil)
  (:method ((number number) &key copied-objects) number)
  (:method ((character character) &key copied-objects) character)
  (:method ((symbol symbol) &key copied-objects) symbol)
  (:method ((pathname pathname) &key copied-objects) pathname)
  (:method ((fn function) &key copied-objects) fn)
  (:method ((weak-pointer sb-ext:weak-pointer) &key copied-objects)
    (let ((copy (sb-ext:make-weak-pointer (%deep-copy (sb-ext:weak-pointer-value weak-pointer) :copied-objects copied-objects))))
      (setf (gethash weak-pointer copied-objects) copy)
      copy))
  ;; not trying to copy anything sensible about SAPs
  (:method ((sap sb-sys:system-area-pointer) &key copied-objects) nil)
  (:method ((struct structure-object) &key copied-objects)
    (let ((new-struct (copy-structure struct))
          (slots (sb-mop:class-direct-slots (class-of struct))))
      (setf (gethash struct copied-objects) new-struct)
      (dolist (slot slots)
        (let ((slot-name (sb-mop:slot-definition-name slot)))
          (setf (slot-value new-struct slot-name)
                (%deep-copy (slot-value struct slot-name) :copied-objects copied-objects))))
      new-struct))
  (:method ((table hash-table) &key (copied-objects))
    (let ((new-table (make-hash-table
                      :test (hash-table-test table)
                      :size (hash-table-size table))))
      (setf (gethash table copied-objects) new-table)
      (maphash #'(lambda(key value)
                   (setf (gethash key new-table) (%deep-copy value :copied-objects copied-objects)))
               table)
      new-table))
  (:method ((sequence sequence) &key (copied-objects))
    (if (and (consp sequence) (or (not (listp (car sequence))) (not (listp (cdr sequence)))))
        ;; maybe it's a cons cell with random stuff in it?
        (let ((copy (cons nil nil)))
          (setf (gethash sequence copied-objects) copy)
          (setf (car copy) (%deep-copy (car sequence) :copied-objects copied-objects)
                (cdr copy) (%deep-copy (cdr sequence) :copied-objects copied-objects))
          copy)
        (let ((copy (copy-seq sequence)))
          (setf (gethash sequence copied-objects) copy)
          (dotimes (i (length sequence))
            (setf (elt copy i)
                  (%deep-copy (elt sequence i) :copied-objects copied-objects)))
          copy)))
  (:method ((array array) &key copied-objects)
    (let* ((element-type (array-element-type array))
           (fill-pointer (and (array-has-fill-pointer-p array)
                              (fill-pointer array)))
           (adjustable (adjustable-array-p array))
           (dimensions (array-dimensions array))
           (new-array (make-array dimensions
                                  :element-type element-type
                                  :adjustable adjustable
                                  :fill-pointer fill-pointer)))
      (setf (gethash array copied-objects) new-array)
      (dotimes (i (array-total-size array))
        (setf (row-major-aref new-array i)
              (%deep-copy (row-major-aref array i) :copied-objects copied-objects)))
      new-array))
  (:method ((object standard-object) &key (copied-objects))
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (setf (gethash object copied-objects) copy)
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (%deep-copy (slot-value object slot-name) :copied-objects copied-objects))))
      copy)))

(defmethod %deep-copy :around (object &key copied-objects)
  (if (and copied-objects (gethash object copied-objects))
      (gethash object copied-objects)
      (let ((copy (call-next-method object :copied-objects copied-objects)))
        (prog1 copy
          (loop :for type
                  :in '(polygon-draw
                        gl-font font-drawable
                        sprite-instance-renderer
                        static-sprite
                        gl-quad
                        audio-sample)
                :do
                   (when (typep copy type)
                     (resource-autoloader-add-object
                      *resource-autoloader*
                      (tg:make-weak-pointer copy))
                     (return)))))))

;; old array code
#+nil
(let* ((element-type (array-element-type array))
           (fill-pointer (and (array-has-fill-pointer-p array)
                              (fill-pointer array)))
           (adjustable (adjustable-array-p array))
           (dimensions (array-dimensions array))
           (new-array (make-array dimensions
                                  :element-type element-type
                                  :adjustable adjustable
                                  :fill-pointer fill-pointer)))
      (dotimes (i (array-total-size array))
        (setf (row-major-aref new-array i)
              (row-major-aref array i)))
      new-array)

@export
(defun deep-copy (object)
  (%deep-copy object :copied-objects (make-hash-table :test #'eq)))
