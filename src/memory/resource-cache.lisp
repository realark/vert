(in-package :recurse.vert)

(defgeneric release (resource)
  (:documentation "Called when a resource will no longer be used.
Will be called on values in a resource-cache when the are removed from the cache."))

(defclass resource-cache (cache)
  ())

(defmethod initialize-entry-metadata ((cache resource-cache) key value)
  (setf (metadata cache key :mtime) (file-write-date key))
  (setf (metadata cache key :objects-using) (make-array 3 :fill-pointer 0 :adjustable T)))

(defparameter *resource-cache*
  (make-instance 'resource-cache
                 :on-evict (lambda (key value)
                             (declare (ignore key))
                             (release value)))
  "System-wide cache of file-based resources.")

(defmacro use-cached-resource (object resource-path &optional creator-form)
  "Inform the system that OBJECT is using the resource at the location of RESOURCE-PATH.
If the resource is not cached, CREATOR-FORM will be eval'd and the result cached."
  (alexandria:once-only (object resource-path)
    `(let ((resource (getcache-default ,resource-path *resource-cache* ,creator-form)))
       (unless (or (null resource)
                   (find ,object (metadata *resource-cache* ,resource-path :objects-using)))
         (vector-push-extend ,object (metadata *resource-cache* ,resource-path :objects-using)))
       resource)))

(defmacro stop-using-cached-resource (object resource-path)
  "Inform the system that OBJECT is no longer using the resource at the location of RESOURCE-PATH"
  (alexandria:once-only (object resource-path)
    (alexandria:with-gensyms (users)
      `(let ((,users (metadata *resource-cache* ,resource-path :objects-using)))
         (when (and ,users (find ,object ,users))
           (setf ,users (delete ,object ,users)
                 (metadata *resource-cache* ,resource-path :objects-using) ,users)
           (when (= 0 (length ,users))
             (remcache ,resource-path *resource-cache*)))))))
