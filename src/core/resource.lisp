;; Resource/Asset loading
(in-package :recurse.vert)

@export
(defgeneric load-resources (object)
  (:documentation "Tell OBJECT to load any external resources (e.g. opengl, audio sfx, etc).
Idempotent. Will be called when all vert systems are initialized.")
  (:method (object)))

@export
(defgeneric release-resources (object)
  (:documentation "Tell OBJECT to release any external resources. Idempotent.")
  (:method (object)))

(defmethod load-resources :around (object)
  (log:debug "loading resources for: ~A" object)
  (prog1 (call-next-method object)
    (log:debug "successful load for: ~A" object)))

(defmethod release-resources :around (object)
  (log:debug "releasing resources for: ~A" object)
  (prog1 (call-next-method object)
    (log:debug "successful release for: ~A" object)))

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
  (:documentation "Tracks all objects with external resources and calls (LOAD|RELEASE)-RESOURCES at the appropriate times."))

(defun resource-autoloader-add-object (resource-autoloader weak-obj-pointer &key skip-object-load)
  "instruct RESOURCE-AUTOLOADER to manage the resources of the object pointed to by WEAK-OBJ-POINTER"
  (unless (tg:weak-pointer-p weak-obj-pointer)
    (error "Resource Manager requires a weak pointer. Given ~A" weak-obj-pointer))
  (with-slots (lock objects can-load-resources-p) resource-autoloader
    (log:debug "Add ~A to autoloaded objects"
               (tg:weak-pointer-value weak-obj-pointer))
    (bt:with-recursive-lock-held (lock)
      (vector-push-extend weak-obj-pointer objects))
    (when (and can-load-resources-p (not skip-object-load))
      (load-resources (tg:weak-pointer-value weak-obj-pointer)))))

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
                   (load-resources object)
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
    (resource-autoloader-release-all resource-autoloader)
    (resource-autoloader-load-all resource-autoloader)))

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

(on-engine-stop ('resource-autoloader-release)
  (setf (resource-autoloader-can-load-resources-p *resource-autoloader*) nil)
  (force-run-all-releasers))

;;;; resource releaser util

;; originally, I had used finalizers to implement resource-releasers
;; this was crashing interactive development in cases where objects
;; try to run their finalizers in the next engine run

(defvar *releaser-finalizers*
  (make-array 100
              :adjustable t
              :fill-pointer 0))

(defclass %resource-releaser ()
  ((label :initarg :label :initform (error ":label required"))
   (finalizer :initarg :finalizer :initform nil)))

(defmethod print-object ((resource-releaser %resource-releaser) out)
  (with-slots (label) resource-releaser
    (print-unreadable-object (resource-releaser out :type t)
      (format out "~A" label))))

@export
(defmacro make-resource-releaser ((&optional object) &body body)
  "Return an instance with a gc finalizer which executes BODY.
OBJECT may be supplied to generate a human-readable name for what is being finalized."
  (alexandria:with-gensyms (label releaser)
    `(let ((,label (format nil "releaser<~A>" ,object)))
       #+nil
       (tg:finalize (make-instance '%resource-releaser
                                   :label (format
                                           nil
                                           ,label
                                           ))
                    (lambda ()
                      (handler-case
                          (progn
                            (log:info "running ~A" ,label)
                            (prog1 (progn ,@body)
                              (log:info "--> finish running ~A" ,label)))
                        (error (e)
                          (log:error "error running <~A> finalizer: ~A"
                                     ,label
                                     e)))))
       (let ((,releaser (make-instance '%resource-releaser
                                       :label (format nil ,label)
                                       :finalizer
                                       (lambda ()
                                         (handler-case
                                             (progn
                                               (log:trace "---- running ~A ---- " ,label)
                                               (prog1 (progn ,@body)
                                                 (log:trace "---- /finish running ~A" ,label)))
                                           (error (e)
                                             (log:error "running <~A> finalizer: ~A"
                                                        ,label
                                                        e)))))))
         (loop :for i :from 0 :below (length *releaser-finalizers*) :by 2 :do
              (when (null (elt *releaser-finalizers* i))
                (setf (elt *releaser-finalizers* i)
                      (tg:make-weak-pointer ,releaser)
                      (elt *releaser-finalizers* (+ i 1))
                      (slot-value ,releaser 'finalizer))
                (return))
            :finally
              (vector-push-extend (tg:make-weak-pointer ,releaser) *releaser-finalizers*)
              (vector-push-extend (slot-value ,releaser 'finalizer) *releaser-finalizers*))
         ,releaser))))

@export
(defun cancel-resource-releaser (resource-releaser)
  "Cancel RESOURCE-RELEASER's pending gc action."
  ;; (tg:cancel-finalization resource-releaser)
  (setf (slot-value resource-releaser 'finalizer)
        nil)
  (loop :for i :from 0 :below (length *releaser-finalizers*) :by 2 :do
       (let ((releaser (when (elt *releaser-finalizers* i)
                           (tg:weak-pointer-value (elt *releaser-finalizers* i))))
             (releaser-fn (elt *releaser-finalizers* (+ i 1))))
         (when (eq resource-releaser releaser)
           (setf (elt *releaser-finalizers* i) nil)
           (setf (elt *releaser-finalizers* (+ i 1)) nil))
         (when (and (null releaser) releaser-fn)
           ;; releaser was GC'd
           (setf (elt *releaser-finalizers* i) nil)
           (setf (elt *releaser-finalizers* (+ i 1)) nil)
           (funcall releaser-fn)))))

(defun force-run-all-releasers ()
  ;; loop the table and run stuff
  (loop :for i :from 0 :below (length *releaser-finalizers*) :by 2 :do
       (let ((releaser-fn (elt *releaser-finalizers* (+ i 1))))
         (setf (elt *releaser-finalizers* i) nil)
         (setf (elt *releaser-finalizers* (+ i 1)) nil)
         (when releaser-fn
           (funcall releaser-fn)))))
