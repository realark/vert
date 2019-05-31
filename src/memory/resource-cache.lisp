(in-package :recurse.vert)

(defclass resource-cache (cache)
  ;; set a default eviction fn
  ((on-evict-fn :initarg :on-evict
                :initform (lambda (key value)
                            (declare (ignore key))
                            (release-resources value))))
  (:documentation "A cache which keeps a count of the number of users of a resource. When the count reaches zero, an entry will be evicted.

Also tracks the file-write date of the key (presumed to be a path to a file).

Useful for scenarios where users of a cache aren't away of who else is using the same resource. For example, we only want to evict a tile texture when the last tile unloads."))

(defmethod initialize-entry-metadata ((cache resource-cache) key value)
  (setf (metadata cache key :mtime) (file-write-date key))
  (setf (metadata cache key :objects-using) (make-array 3 :fill-pointer 0 :adjustable T)))

(defmacro use-cached-resource (object resource-path resource-cache &optional creator-form)
  "Inform RESOURCE-CACHE that OBJECT is using the resource at the location of RESOURCE-PATH.
If the resource is not cached, CREATOR-FORM will be eval'd and the result cached."
  (alexandria:once-only (object resource-path resource-cache)
    (alexandria:with-gensyms (resource)
      `(let ((,resource (getcache-default ,resource-path ,resource-cache ,creator-form)))
         (unless (or (null ,resource)
                     (find ,object (metadata ,resource-cache ,resource-path :objects-using)))
           (vector-push-extend ,object (metadata ,resource-cache ,resource-path :objects-using)))
         ,resource))))

(defmacro stop-using-cached-resource (object resource-path resource-cache)
  "Inform RESOURCE-CACHE that OBJECT is no longer using the resource at the location of RESOURCE-PATH"
  (alexandria:once-only (object resource-path resource-cache)
    (alexandria:with-gensyms (users)
      `(let ((,users (metadata ,resource-cache ,resource-path :objects-using)))
         (when (and ,users (find ,object ,users))
           (setf ,users (delete ,object ,users)
                 (metadata ,resource-cache ,resource-path :objects-using) ,users)
           (when (= 0 (length ,users))
             (remcache ,resource-path ,resource-cache)))))))
