;; Resource/Asset loading

(in-package :recurse.vert)

@export
(defvar *default-resource-dir* "./resources"
  "The default relative path checked during resource lookup.")

@export
(defvar *config-resource-dirs-key* "resource-dirs")

(let ((media-dir (if swank::*emacs-connection*
                     (namestring ; running in emacs
                      (asdf:system-relative-pathname 'vert #p"media/"))
                     "media/"))
      (test-media-dir (namestring
                       (asdf:system-relative-pathname 'vert #p"t/media/")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %resource-path (resource-relative-path &optional error-if-absent)
    (labels ((absolute-resource-path (resource-dir resource-relative-path)
               (concatenate 'string resource-dir "/" resource-relative-path))
             (resource-exists-p (resource-dir resource-relative-path)
               (probe-file (absolute-resource-path resource-dir resource-relative-path))))
      (or
       (loop :for resource-dir :in (getconfig *config-resource-dirs-key* *engine-config*) :do
            (when (resource-exists-p resource-dir resource-relative-path)
              (return (absolute-resource-path resource-dir resource-relative-path))))
       (when (resource-exists-p *default-resource-dir* resource-relative-path)
         (absolute-resource-path *default-resource-dir* resource-relative-path))
       (when error-if-absent
         (error "~A not found. Checked ~A and ~A"
                resource-relative-path
                (getconfig *config-resource-dirs-key* *engine-config*)
                *default-resource-dir*))))))

@export
(defmacro resource-path (resource-relative-path)
  "Given a resource name, returns a path to locate the resource."
  (if (stringp resource-relative-path)
      (%resource-path resource-relative-path T)
      `(%resource-path ,resource-relative-path)))

(defun test-resource-path (resource-name)
  "Given a resource name, returns a path to locate the test resource."
  (concatenate 'string
               (namestring
                (asdf:system-relative-pathname 'vert #p"t/media/"))
               resource-name))
