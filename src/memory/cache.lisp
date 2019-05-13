(in-package :recurse.vert)

(defclass cache ()
  ((on-evict-fn :initarg :on-evict
                :initform (lambda (key value)
                            (declare (ignore key) (ignore value))
                            (values))
                :documentation "A function which will run on a value after it is removed from the cache.")
   (test-fn :initarg :test
            :initform #'equalp
            :reader test-fn
            :documentation "Function to test for key equality. Takes 2 key arguments.")
   (htable :initform nil
           :documentation "Backing Hash Table. key -> (metadata . value)"))
  (:documentation "A wrapper around a hash table Used for caching game resources."))

(defmethod initialize-instance :after ((cache cache) &rest args)
  (declare (ignore args))
  (setf (slot-value cache 'htable) (make-hash-table
                                    :test (test-fn cache))))

(defgeneric metadata (cache key meta-key)
  (:documentation "Get the value of META-KEY associated with CACHE's KEY entry.")
  (:method ((cache cache) key meta-key)
    (let* ((hval (gethash key (slot-value cache 'htable)))
           (metalist (when hval (car hval))))
      (when metalist
        (getf metalist meta-key)))))

(defmethod (setf metadata) (value (cache cache) key meta-key)
  (let* ((hval (gethash key (slot-value cache 'htable)))
         (metalist (when hval (car hval))))
    (when hval
      (setf (getf metalist meta-key) value)
      (setf (car hval) metalist))))

(defgeneric initialize-entry-metadata (cache key value)
  (:documentation "Initialize the metadata for an entry.")
  (:method ((cache cache) key value)))

(defmacro getcache-default (key cache &optional initializer)
  "Returns the value associated with KEY in CACHE.
If provided, INITIALIZER will be eval'd to populate the cache value
for KEY (as long as there is no existing value)"
  (alexandria:once-only (key cache)
    (alexandria:with-gensyms (result)
      `(let ((,result (getcache ,key ,cache)))
         (if ,result
             ,result
             (setf (getcache ,key ,cache) ,initializer))))))

(defgeneric getcache (key cache)
  (:method (key (cache cache))
    (let ((hval (gethash key (slot-value cache 'htable))))
      (when hval
        (cdr hval)))))

(defgeneric (setf getcache) (value key cache)
  (:method (value key (cache cache))
    (with-slots ((table htable) (on-evict-fn on-evict-fn)
                 (max max-size))
        cache
      (let ((old-entry (gethash key table)))
        (setf (gethash key table)
              (cons (list) value))
        (initialize-entry-metadata cache key value)
        (when old-entry
          (funcall on-evict-fn key (cdr old-entry)))
        value))))

(defmethod remcache (key (cache cache))
  (with-slots ((table htable) (on-evict-fn on-evict-fn)
               (max max-size))
      cache
    (let ((old-entry (gethash key table)))
      (when old-entry
        (funcall on-evict-fn key (cdr old-entry))))
    (remhash key table)))

@export
(defmethod clear-cache ((cache cache))
  (with-slots ((table htable) (on-evict-fn on-evict-fn))
      cache
    (maphash (lambda (key value)
               (remhash key table)
               (funcall on-evict-fn key (cdr value)))
             table)))

(defmethod cache-count ((cache cache))
  (hash-table-count (slot-value cache 'htable)))

(defmacro %do-cache ((cache key-binding value-binding &rest metadata-bindings) &body body)
  "Iterate CACHE with key, value, and metadata bindings.
METADATA-BINDINGS should be keyword symbol pars. E.g. :foo foo-binding"
  (assert (symbolp key-binding))
  (assert (symbolp value-binding))
  (assert (evenp (length metadata-bindings)))
  (alexandria:once-only (cache)
    (alexandria:with-gensyms (htable hval metalist)
      `(with-slots ((,htable htable)) ,cache
         (maphash (lambda (,key-binding ,hval)
                    (let* ((,metalist (car ,hval))
                           (,value-binding (cdr ,hval))
                           ,@(loop for (meta-key meta-binding) on metadata-bindings by #'cddr
                                do
                                  (assert (keywordp meta-key))
                                  (assert (symbolp meta-binding))
                                collect `(,meta-binding (getf ,metalist ,meta-key))))
                      ,@body))
                  ,htable)))))

@export
(defmacro do-cache ((cache key-binding value-binding) &body body)
  "Iterate over CACHE with KEY-BINDING and VALUE-BINDING."
  `(%do-cache (,cache ,key-binding ,value-binding)
     ,@body))

@export
(defvar *engine-caches* (make-instance 'cache
                                        :on-evict (lambda (cache-name cache)
                                                    (declare (ignore cache-name))
                                                    (clear-cache cache)))
  "Global Cache of all caches used by the engine.")
