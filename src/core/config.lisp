(in-package :recurse.vert)

@export-class
(defclass config ()
  ((%config-hash :initform (make-hash-table)))
  (:documentation "Immutable. Conceptually a map of keys (symbol) to values (T)"))

@export
(defun getconfig (key config)
  (declare (symbol key)
           (config config))
  (gethash key (slot-value config '%config-hash)))

@export
(defmacro make-config (&rest key-value-pairs)
  "Create a config out of KEY-VALUE-PAIRS.

Each pair is a two value list with the first value being a symbol.

Example:
  (make-config ('foo \"foo val\") ('bar \"bar val\"))"
  (alexandria:with-gensyms (config hash)
    `(let ((,config (make-instance 'config)))
       (with-slots ((,hash %config-hash)) ,config
         ,@(loop :with config-setters = (list)
              :for key-val :in key-value-pairs :do

                (unless (and (listp key-val) (= 2 (length key-val)))
                  (error "key-value-pairs must be two arg list of (key val). Got ~A" key-val))
                (push `(setf (gethash (runtime-type-assert
                                       ,(first key-val)
                                       'symbol
                                       "config keys must be symbols")
                                      ,hash)
                             ,(second key-val))
                      config-setters)
              :finally (return (nreverse config-setters)))
         ,config))))

@export
(defun export-config-key (symbol &optional documentation)
  (export (list symbol))
  (when documentation
    (setf (documentation symbol 'variable) documentation)))

(export-config-key
 'config-resource-dir "Dirs to check when looking for a resource. See RESOURCE-PATH.")

@export
(defvar *config* (make-config
                  ('config-resource-dirs (list "./resources")))
  "The active config used by vert.")
