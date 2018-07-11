(in-package :recurse.vert/test)

(defun run-prove-tests (&rest packages)
  "Reset counts and run all tests.
Returns T if all tests in all packages pass.
If a test in a package fails testing is aborted and nil returned."
  (assert (> (length packages) 0))
  (let ((original-package (package-name *package*)))
    (unwind-protect
         (loop for pack in packages do
            ;; zero out existing counts
              (eval `(in-package ,pack))
              (prove:plan nil)
              (unless (prove:run-test-package pack)
                (return nil))
            finally (return T))
      (eval `(in-package ,original-package)))))
