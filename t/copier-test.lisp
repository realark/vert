(in-package :recurse.vert)

(prove:deftest copier-test
  (loop :for obj :in (list 27 nil t :stuff 'a (make-pathname :directory ".") #\f) :do
    (prove:is obj (deep-copy obj) "this type should not be cloned" :test #'eq))
  (let ((foreign-pointer (cffi:foreign-alloc :int)))
    (unwind-protect
         (prove:is nil (deep-copy foreign-pointer) "foreign pointers should not be copied")
      (cffi:foreign-free foreign-pointer)))

  (let ((struct (make-color :a 0.0)))
    (prove:isnt struct (deep-copy struct) "should be a different instance" :test #'eq)
    (prove:is struct (deep-copy struct) "should be the same data" :test #'equalp))

  (let ((obj (make-instance 'game-object :object-name "foobar")))
    (prove:isnt obj (deep-copy obj) "should be a different instance" :test #'eq)
    (prove:is obj (deep-copy obj) "should be the same data"
              :test (lambda (o1 o2)
                      (equalp (object-name o1)
                              (object-name o2)))))

  (let* ((arr (make-array 3 :initial-contents
                          (list (make-color :g 0.7) (make-color :r 0.12) (make-color :b 0.23))
                          ))
         (arr-clone (deep-copy arr)))
    (prove:isnt arr arr-clone "should be a different instance" :test #'eq)
    (loop :for i :from 0 :below (length arr) :do
      (let ((struct (elt arr i))
            (struct-clone (elt arr-clone i)))
        (prove:isnt struct struct-clone "should be a different instance" :test #'eq)
        (prove:is struct struct-clone "should be the same data" :test #'equalp)))))
