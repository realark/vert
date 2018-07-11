(in-package :recurse.vert/unit-test)

(deftest base-cache
  (let* ((last-evicted-value nil)
         (num-evictions 0)
         (cache (make-instance 'cache
                               :on-evict (lambda (key value)
                                           (declare (ignore key))
                                           (incf num-evictions)
                                           (setf last-evicted-value value)))))
    (is (getcache 1 cache) nil "No key in cache.")
    (setf (getcache 1 cache) 'one)
    (setf (getcache 2 cache) 'two)
    (is (getcache 1 cache) 'one "Expected value in cache")
    (is (cache-count cache) 2 "Correct cache count.")

    (remcache 1 cache)
    (is last-evicted-value 'one "One should have been evicted")
    (is (cache-count cache) 1 "Correct cache count post removal.")
    (is last-evicted-value 'one "One should have been evicted")

    (setf (getcache 2 cache) 'too)
    (is (cache-count cache) 1 "Update does not increment count.")
    (is last-evicted-value 'two "Updating cache value evicts old value.")

    (clear-cache cache)
    (is (cache-count cache) 0 "cache cleared")
    (is last-evicted-value 'too "Clearing runs eviction.")))

(deftest cache-evict-oldest
  (let* ((last-evicted-value nil)
         (num-evictions 0)
         (cache (make-instance 'evict-oldest-cache
                               :max-size 5
                               :on-evict (lambda (key value)
                                           (declare (ignore key))
                                           (incf num-evictions)
                                           (setf last-evicted-value value)))))
    (setf (getcache 1 cache) 'one)
    (sleep .001)
    (setf (getcache 2 cache) 'two)
    (sleep .001)
    (setf (getcache 3 cache) 'three)
    (sleep .001)
    (setf (getcache 4 cache) 'four)
    (sleep .001)
    (setf (getcache 5 cache) 'five)
    (sleep .001)
    (setf (getcache 6 cache) 'six)
    (is (cache-count cache) 5 "Cache capped at max size.")
    (is last-evicted-value 'one "One should have been evicted")

    (getcache 2 cache) ; getcache updates access timestamp
    (setf (getcache 7 cache) 'seven)
    (is last-evicted-value 'three "Three should have been evicted")
    (clear-cache cache)
    (is (cache-count cache) 0)
    (is num-evictions 7 "Six evictions total")))

(deftest counting-cache
  (let* ((last-evict nil)
         (cache (make-instance 'counting-cache
                               :on-evict (lambda (key val)
                                           (setf last-evict (cons key val))))))
    (setf (getcache 'foo cache) "fooval")
    (getcache-default 'foo cache (fail "initializer should not run"))
    (getcache 'foo cache)
    (getcache-default 'bar cache "barval")
    (remcache 'not-in-cache cache)
    (prove:is last-evict nil "remcache no effect for uncached keys")

    (remcache 'foo cache)
    (prove:is last-evict nil "don't remove at count 2")
    (remcache 'foo cache)
    (prove:is last-evict nil "don't remove at count 1")
    (remcache 'foo cache)
    (prove:is last-evict (cons 'foo "fooval")
              "remove at count 0"
              :test #'equalp)))
