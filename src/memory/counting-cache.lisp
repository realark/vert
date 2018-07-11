(in-package :recurse.vert)

;; counting-cache
(defclass counting-cache (cache)
  ()
  (:documentation "A cache which counts all keys in use and evicts entries with a count of 0."))

(defmethod initialize-entry-metadata ((cache counting-cache) key value)
  (setf (metadata cache key :use-count) 1))

(defmethod getcache :after (key (cache counting-cache))
  (with-slots (htable) cache
    (let ((entry (gethash key htable)))
      (when entry
        (incf (metadata cache key :use-count))))))

(defmethod remcache (key (cache counting-cache))
  (let ((count (metadata cache key :use-count)))
    (when count
      (if (<= count 1)
          (call-next-method key cache)
          (decf (metadata cache key :use-count))))))
