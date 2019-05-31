(in-package :recurse.vert)

(defclass evict-oldest-cache (cache)
  ((max-size :initarg :max-size
             :initform 0
             :reader max-size
             :documentation "Size the cache will grow to before running the eviction fn.
                             0 or less will grow unbounded."))
  (:documentation "A cache which evicts the oldest value (pulled out with getcache) when it reaches MAX-SIZE"))

(defmethod initialize-entry-metadata ((cache evict-oldest-cache) key value)
  (declare (ignore value))
  (setf (metadata cache key :creation-time) (ticks)))

(defmethod getcache :after (key (cache evict-oldest-cache))
  (setf (metadata cache key :creation-time) (ticks)))

(defmethod (setf getcache) :before (value key (cache evict-oldest-cache))
  (with-slots (max-size) cache
    (when (and (> max-size 0)
               (= (cache-count cache) max-size))
      (%remove-oldest cache))))

(defun %remove-oldest (cache)
  (declare (type evict-oldest-cache cache))
  (let ((oldest-key nil)
        (oldest-ts 0))
    (with-slots ((table htable) (on-evict-fn on-evict-fn))
        cache
      (do-cache-with-metadata (cache key value :creation-time ctime)
        (declare (ignore value))
        (when (or (null oldest-key)
                  (< ctime oldest-ts))
          (setf oldest-key key)
          (setf oldest-ts ctime)))
      (remcache oldest-key cache))))
