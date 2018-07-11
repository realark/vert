;; Resource/Asset loading

(in-package :recurse.vert)

(let ((media-dir (if swank::*emacs-connection*
                     (namestring ; running in emacs
                      (asdf:system-relative-pathname 'vert #p"media/"))
                     "media/"))
      (test-media-dir (namestring
                       (asdf:system-relative-pathname 'vert #p"t/media/"))))

  (defun resource-path (resource-name)
    "Given a resource name, returns a path to locate the resource."
    (concatenate 'string media-dir resource-name))

  (defun test-resource-path (resource-name)
    "Given a resource name, returns a path to locate the test resource."
    (concatenate 'string test-media-dir resource-name)))
