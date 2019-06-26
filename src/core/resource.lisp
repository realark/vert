;; Resource/Asset loading

(in-package :recurse.vert)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %resource-path (resource-relative-path &optional error-if-absent)
    (labels ((absolute-resource-path (resource-dir resource-relative-path)
               (concatenate 'string resource-dir "/" resource-relative-path))
             (resource-exists-p (resource-dir resource-relative-path)
               (probe-file (absolute-resource-path resource-dir resource-relative-path))))
      (or
       (loop :for resource-dir :in (getconfig 'config-resource-dirs *config*) :do
            (when (resource-exists-p resource-dir resource-relative-path)
              (return (absolute-resource-path resource-dir resource-relative-path))))
       (when error-if-absent
         (error "~A not found. Checked dir(s): ~A"
                resource-relative-path
                (getconfig 'config-resource-dirs *config*)))))))

@export
(defmacro resource-path (resource-relative-path)
  "Given a resource name, returns a path to locate the resource."
  `(%resource-path ,resource-relative-path))

(defun test-resource-path (resource-name)
  "Given a resource name, returns a path to locate the test resource."
  (concatenate 'string
               (namestring
                (asdf:system-relative-pathname 'vert #p"t/media/"))
               resource-name))
