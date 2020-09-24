(in-package :recurse.vert)

(defparameter %test-thing-pub-body-invokes% (list))
(defparameter %test-thing-sub-body-invokes% (list))
(defparameter %test-thing-global-invokes% (list))

(defclass test-pub (event-publisher) ())

(defclass test-sub () ())

(defevent test-thing (pub arg1 arg2)
    "A test event"
  (push (list pub arg1 arg2)
        %test-thing-pub-body-invokes%))

(defevent-handler test-thing ((pub test-pub) (sub test-sub) arg1 arg2)
    "Some doc"
  (push (list pub sub arg1 arg2)
        %test-thing-sub-body-invokes%))

(defevent-handler-global global-test-thing (test-thing pub arg1 arg2)
    "Some doc"
  (push (list pub arg1 arg2)
        %test-thing-global-invokes%))

(prove:deftest test-events-do-not-cons
  (block warm-up
    (loop :for i :from 1 :below 1024 :do
      (event-publish 'input-happened
                     *standard-input*
                     'arg-one
                     'arg-two
                     'arg-three
                     'arg-four
                     'arg-five)
          :finally
             (events-run-pending)
             (events-run-pending)))
  (garbage-collect-block)

  (loop :with pub-size = 256
        :and starting-heap-size = (sb-kernel:dynamic-usage)
        :and test-cons-max-bytes = 10
        :and starting-gc-count = (vert::current-gc-count)
        :for i :from 1 :below 300000 :do
          (event-publish 'input-happened
                         *standard-input*
                         'arg-one
                         'arg-two
                         'arg-three
                         'arg-four
                         'arg-five)
          (when (and (> i 0) (= (mod i pub-size) 0))
            (assert (= (length %pending-events%) pub-size))
            (events-run-pending)
            (assert (= (length %pending-events%) 0)))
        :finally
           (prove:is (vert::current-gc-count) starting-gc-count
                     "No Garbage Collection occurred.")
           (prove:is (- (sb-kernel:dynamic-usage) starting-heap-size)
                     test-cons-max-bytes
                     :test #'<=
                     (format t "No more than ~A bytes consed." test-cons-max-bytes))))

(prove:deftest test-events-low-level
  (events-run-pending)

  (let ((%test-thing-pub-body-invokes% (list))
        (%test-thing-sub-body-invokes% (list))
        (%test-thing-global-invokes% (list))
        (sub1 (make-instance 'test-sub))
        (sub2 (make-instance 'test-sub))
        (sub3 (make-instance 'test-sub))
        (pub (make-instance 'test-pub))
        (num-events 100))
    (event-subscribe pub sub1 test-thing)
    (event-subscribe pub sub2 test-thing)
    (event-subscribe pub sub3 test-thing some-other-event)
    (prove:is (slot-value pub 'event-subscribers)
              (%make-hash (list 'test-thing (vector sub1 sub2 sub3)
                                'some-other-event (vector sub3)))
              :test #'%hash-tables-equalp
              "All subs in subscriber list")
    (event-unsubscribe pub sub3 test-thing)
    (prove:is (slot-value pub 'event-subscribers)
              (%make-hash (list 'test-thing (vector sub1 sub2)
                                'some-other-event (vector sub3)))
              :test #'%hash-tables-equalp
              "sub3 removed from test-thing event")
    ;; (event-subscribe pub sub3 test-thing some-other-event)
    ;; (event-unsubscribe pub sub3 test-thing)
    (loop :for i :from 0 :below num-events :do
      (event-publish 'test-thing pub 1 2))
    (events-run-pending)

    (prove:is (length %test-thing-pub-body-invokes%) 100)
    (prove:is (first %test-thing-pub-body-invokes%)
              (list pub 1 2)
              :test #'equalp)
    (prove:is %test-thing-pub-body-invokes%
              (make-list num-events
                         :initial-element
                         (list pub 1 2))
              :test #'equalp)
    (prove:is (length %test-thing-sub-body-invokes%) (* 2 num-events)
              "Each subscribed sub receives event handler callback.")

    ;; expecting the sub invoke list to alternate between sub1 and sub2
    (loop :with expected-sub = (second (first %test-thing-sub-body-invokes%))
          :for i :from 0
          :for invoke :in  %test-thing-sub-body-invokes% :do
            (unless (eq (second invoke) expected-sub)
              (prove:fail
               (format nil "~A :: (1: ~A, 2: ~A) expected to find sub ~A, but found ~A instead"
                       i sub1 sub2
                       expected-sub
                       (second invoke)))
              (return))
            (log:trace "~A (1: ~A, 2: ~A) Found expected sub: ~A"
                       i sub1 sub2
                       expected-sub)
            (if (eq sub1 expected-sub)
                (setf expected-sub sub2)
                (setf expected-sub sub1))
          :finally
             (prove:pass "Subs invoked in expected order."))

    (prove:is %test-thing-global-invokes% %test-thing-pub-body-invokes%
              :test #'equalp
              ;; global handler body produces the same list structure as the pub list
              "Global handler invoked")))

(defun %make-hash (plist)
  "Make a hash table out of PLIST"
  (let ((h (make-hash-table :test #'eq)))
    (loop :for (k v) :on plist :by #'cddr :while v :do
      (setf (gethash k h) v))
    h))

(defun %hash-tables-equalp (hash1 hash2)
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (loop :for key1 :being :the hash-keys :of hash1
               :using (hash-value value1) :do
                 ;; remove impl null padding
                 (let ((value1 (remove-if #'null value1))
                       (value2 (remove-if #'null (gethash key1 hash2 #()))))
                   (unless (and (= (length value1)
                                   (length value2))
                                (loop :for v1 :across value1
                                      :for v2 :across value2 :do
                                        (unless (equalp v1 v2)
                                          (return nil))
                                      :finally
                                         (return t)))
                     (return nil)))
             :finally
                (return t))))
