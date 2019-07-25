(in-package :recurse.vert/test)

(defun vert-test-fn (obj1 obj2)
  "Compare numbers by float value out to 5 decimal places"
  (if (and (realp obj1) (realp obj2))
      (float= obj1 obj2 :precision 5)
      (equal obj1 obj2)))

(defun run-prove-tests (&key (reporter :fiveam) packages tests)
  "Reset counts and run all tests. T if all tests pass."
  (loop :for test-symb :in tests :do
       (let ((prove:*default-test-function* #'vert-test-fn)
             (prove:*default-reporter* reporter)
             (*package* (symbol-package test-symb)))
         ;; zero out existing counts
         (prove:plan nil)
         (unless (prove:run-test test-symb)
           (return nil))))
  (loop :for pack :in packages :do
       (let ((*package* (find-package pack))
             (prove:*default-test-function* #'vert-test-fn)
             ;; list :dot :tap :fiveam
             (prove:*default-reporter* reporter))
         ;; zero out existing counts
         (prove:plan nil)
         (unless (prove:run-test-package pack)
           (return nil)))
     :finally (return T)))

#+nil
(recurse.vert/test:run-prove-tests :packages '(:recurse.vert/test))

#+nil
(recurse.vert/test:run-prove-tests :reporter :list :tests '(recurse.vert/test::base-cache))
