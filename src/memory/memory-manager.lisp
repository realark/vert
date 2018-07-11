(in-package :recurse.vert)

(defclass memory-manager ()
  ((caches :initarg :caches
           :initform '()
           :reader caches
           :documentation "Caches in use throughout the engine."))
  (:documentation "Distributes and tracks data structures used for custom memory management."))

(defparameter *memory-manager* (make-instance 'memory-manager)
  "Global Memory-Manager. Outside of the game engine, this must be accessed through the global engine-manager.")

(defgeneric register-cache (memory-manager cache-name cache)
  (:documentation "Register CACHE named CACHE-NAME with MEMORY-MANAGER.
Returns CACHE.")
  (:method ((memory-manager memory-manager) (cache-name string) (cache cache))
    (with-slots (caches) memory-manager
      (when (assoc cache-name caches)
        (error "~A already registered in ~A" cache-name memory-manager))
      (push (cons cache-name cache) (slot-value memory-manager 'caches))
      cache)))

(defgeneric get-registered-cache (memory-manager cache-name)
  (:documentation "Return a cache named CACHE-NAME previously registered with MEMORY-MANAGER")
  (:method ((memory-manager memory-manager) (cache-name string))
    (with-slots (caches) memory-manager
      (let ((cache (assoc cache-name caches :test #'equalp)))
        (when cache
          (cdr cache))))))

(defgeneric deregister-cache (memory-manager cache-name)
  (:method ((memory-manager memory-manager) (cache-name string))
    (with-slots (caches) memory-manager
      (let ((cache (assoc cache-name caches :test #'equalp)))
        (unless cache
          (error "~A not registered in ~A" cache-name memory-manager))
        (setf caches (delete cache-name caches :key #'car :test #'equalp))
        (cdr cache)))))
