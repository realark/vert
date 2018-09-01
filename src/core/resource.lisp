;; Resource/Asset loading

(in-package :recurse.vert)

(let ((media-dir (if swank::*emacs-connection*
                     (namestring ; running in emacs
                      (asdf:system-relative-pathname 'vert #p"media/"))
                     "media/"))
      (test-media-dir (namestring
                       (asdf:system-relative-pathname 'vert #p"t/media/"))))

  (defmacro resource-path (resource-name)
    "Given a resource name, returns a path to locate the resource."
    (when (stringp resource-name)
      (let ((file-path (concatenate 'string media-dir resource-name)))
        (unless (probe-file file-path)
          (error (format nil "No resource file: ~A" file-path)))))
    `(%resource-path ,resource-name))

  (defun %resource-path (resource-name)
    (concatenate 'string media-dir resource-name))

  (defun test-resource-path (resource-name)
    "Given a resource name, returns a path to locate the test resource."
    (concatenate 'string test-media-dir resource-name)))
