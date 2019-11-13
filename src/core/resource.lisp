;; Resource/Asset loading
(in-package :recurse.vert)

;;;; getting the path to resources

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %resource-path (resource-relative-path &optional error-if-absent)
    (labels ((absolute-resource-path (resource-dir resource-relative-path)
               (concatenate 'string resource-dir "/" resource-relative-path))
             (resource-exists-p (resource-dir resource-relative-path)
               (probe-file (absolute-resource-path resource-dir resource-relative-path))))
      (or
       (loop :for resource-dir :in (getconfig 'config-resource-dirs *config*) :do
            (when (resource-exists-p resource-dir resource-relative-path)
              (return (absolute-resource-path resource-dir resource-relative-path))))
       (when error-if-absent
         (error "~A not found. Checked dir(s): ~A"
                resource-relative-path
                (getconfig 'config-resource-dirs *config*)))))))

@export
(defmacro resource-path (resource-relative-path)
  "Given a resource name, returns a path to locate the resource."
  `(%resource-path ,resource-relative-path))

(defun test-resource-path (resource-name)
  "Given a resource name, returns a path to locate the test resource."
  (concatenate 'string
               (namestring
                (asdf:system-relative-pathname 'vert #p"t/media/"))
               resource-name))

;;;; automatic loading and releasing of external resources and foreign memory

(defclass resource-autoloader ()
  ((lock :initform (bt:make-recursive-lock "resource-autoloader-lock"))
   (can-load-resources-p :initform nil
                         :accessor resource-autoloader-can-load-resources-p
                         :documentation "When T, foreign resources may be loaded")
   (objects :initform (make-array 100 :fill-pointer 0)))
  (:documentation "Tracks all objects with external resources and calls (LOAD|RELEASE)-RESOURCES at the appropriate times.
All GAME-OBJECTs automatically use the resource-autoloader (see game-object for details).
This means game code will simply define load/release resource methods and not have to worry about calling them."))

(defun resource-autoloader-add-object (resource-autoloader weak-obj-pointer)
  "instruct RESOURCE-AUTOLOADER to manage the resources of the object pointed to by WEAK-OBJ-POINTER"
  (unless (tg:weak-pointer-p weak-obj-pointer)
    (error "Resource Manager requires a weak pointer. Given ~A" weak-obj-pointer))
  (with-slots (lock objects can-load-resources-p) resource-autoloader
    (log:debug "Add ~A to autoloaded objects"
               (tg:weak-pointer-value weak-obj-pointer))
    (bt:with-recursive-lock-held (lock)
      (vector-push-extend weak-obj-pointer objects))
    (when can-load-resources-p
      (load-resources (tg:weak-pointer-value weak-obj-pointer)
                      *gl-context*))))

(defun resource-autoloader-remove-object (resource-autoloader weak-obj-pointer)
  "instruct RESOURCE-AUTOLOADER to stop managing the resources of the object pointed to by WEAK-OBJ-POINTER"
  (unless (tg:weak-pointer-p weak-obj-pointer)
    (error "Resource Manager requires a weak pointer. Given ~A" weak-obj-pointer))
  (with-slots (lock objects can-load-resources-p) resource-autoloader
    (let ((object (tg:weak-pointer-value weak-obj-pointer)))
      (log:debug "Remove ~A from autoloaded objects" object)
      (bt:with-recursive-lock-held (lock)
        (setf objects (delete weak-obj-pointer objects)))
      (when object
        (release-resources object)))))

(defmethod (setf resource-autoloader-can-load-resources-p) :around (new-value (autoloader resource-autoloader))
  (let ((old-value (resource-autoloader-can-load-resources-p autoloader)))
    (prog1 (call-next-method new-value autoloader)
      (let ((new-value (resource-autoloader-can-load-resources-p autoloader)))
        (unless (equal new-value old-value)
          (if new-value
              (resource-autoloader-load-all autoloader)
              (resource-autoloader-release-all autoloader)))))))

(defun resource-autoloader-load-all (resource-autoloader)
  (with-slots (lock objects can-load-resources-p) resource-autoloader
    (when can-load-resources-p
      (bt:with-recursive-lock-held (lock)
        (loop :for i :from 0
             :with empty-refs-p = nil
           :for pointer :across objects :do
             (let ((object (tg:weak-pointer-value pointer)))
               (if object
                   (load-resources object *gl-context*)
                   (setf empty-refs-p t
                         (elt objects i) nil)))
           :finally
             (when empty-refs-p
               (setf objects
                     (delete nil objects))))))))

(defun resource-autoloader-release-all (resource-autoloader)
  (with-slots (lock objects) resource-autoloader
    (bt:with-recursive-lock-held (lock)
      (loop :for i :from 0
         :with empty-refs-p = nil
         :for pointer :across objects :do
           (let ((object (tg:weak-pointer-value pointer)))
             (if object
                 (release-resources object)
                 (setf empty-refs-p t
                       (elt objects i) nil)))
         :finally
           (when empty-refs-p
             (setf objects
                   (delete nil objects)))))))

(defun resource-autoloader-reload-all (resource-autoloader)
  (bt:with-recursive-lock-held ((slot-value resource-autoloader 'lock))
    (resource-autoloader-load-all resource-autoloader)
    (resource-autoloader-release-all resource-autoloader)))

(defun resource-autoloader-prune-empty-refs (resource-autoloader)
  (with-slots (lock objects) resource-autoloader
    (bt:with-recursive-lock-held (lock)
      (loop :for i :from 0
         :with empty-refs-p = nil
         :for pointer :across objects :do
           (unless (tg:weak-pointer-value pointer)
             (setf empty-refs-p t
                   (elt objects i) nil))
         :finally
           (when empty-refs-p
             (setf objects
                   (delete nil objects)))))))

(defvar *resource-autoloader* (make-instance 'resource-autoloader))

(on-engine-start ('resource-autoloader-load)
 (setf (resource-autoloader-can-load-resources-p *resource-autoloader*) t))

(on-engine-stop ('resource-autoloader-load)
  (sb-ext:gc :full t)
  (setf (resource-autoloader-can-load-resources-p *resource-autoloader*) nil))
