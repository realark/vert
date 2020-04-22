(in-package :recurse.vert)

(defun %vert-test-fn (obj1 obj2)
  "Compare numbers by float value out to 5 decimal places"
  (if (and (realp obj1) (realp obj2))
      (float= (coerce obj1 'single-float)
              (coerce obj2 'single-float)
              :precision 5)
      (equal obj1 obj2)))

@export
(defun run-tests (&key
                    reporter
                    (packages (list))
                    (tests (list))
                    debug-on-error)
  "Reset counts and run all tests. T if all tests pass.
REPORTER is a prove reporter. If null the test runner will guess which one is best to use. Interesting reporters: :fiveam :list

Examples:
;; run a suite of tests associated with a package
\(run-tests :packages '\(:recurse.vert\)\)

;; run a single test with verbose output
\(run-tests :reporter :list :tests '\(vert::base-cache-test\)\)"
  (when (not (listp packages))
    (setf packages (list packages)))
  (when (not (listp tests))
    (setf tests (list tests)))
  (unless (or tests packages)
    (error "must specify :TESTS and/or :PACKAGES."))
  (unless reporter
    (setf reporter
          (if packages
              ;; full package runs have a lot of output. Report fiveam to condense output.
              :fiveam
              :list)))
  (and
   (loop :for test-symb :in tests :do
        (format T "~%----~A----~%" test-symb)
        (let ((prove:*default-test-function* #'%vert-test-fn)
              (prove:*default-reporter* reporter)
              (prove:*debug-on-error* debug-on-error)
              (*package* (symbol-package test-symb)))
          ;; zero out existing counts
          (prove:plan nil)
          (prove:run-test test-symb)
          (unless (prove:finalize)
            (return nil)))
      :finally (return T))
   (loop :for pack :in packages :do
        (format T "~%----~A----~%" pack)
        (let ((*package* (find-package pack))
              (prove:*default-test-function* #'%vert-test-fn)
              (prove:*debug-on-error* debug-on-error)
              ;; list :dot :tap :fiveam
              (prove:*default-reporter* reporter))
          ;; zero out existing counts
          (prove:plan nil)
          (prove:run-test-package pack)
          (unless (prove:finalize)
            (return nil)))
      :finally (return T))))
