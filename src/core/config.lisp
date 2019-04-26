(in-package :recurse.vert)

@export-class
(defclass config ()
  ((%config-hash :initform (make-hash-table :test #'equalp)))
  (:documentation "Immutable. Conceptually a map of keys (case-insensitive strings) to values (T)"))

@export
(defun getconfig (key config)
  (declare (string key)
           (config config))
  (gethash key (slot-value config '%config-hash)))

@export
(defmacro make-config (&rest key-value-pairs)
  "Create a config out of KEY-VALUE-PAIRS.

Each pair is a two value list with the first value being a string.

Examples:
  (make-config (\"foo\" 'foo-val) (\"bar\" bar-val))
  (make-config ((format nil \"foo\") 'foo-val) (\"bar\" bar-val))"
  (alexandria:with-gensyms (config hash)
    `(let ((,config (make-instance 'config)))
       (with-slots ((,hash %config-hash)) ,config
         ,@(loop :with config-setters = (list)
              :for key-val :in key-value-pairs :do
                (unless (and (listp key-val) (= 2 (length key-val)))
                  (error "key-value-pairs must be two arg list of (key val). Got ~A" key-val))
                (push `(setf (gethash ,(runtime-type-assert
                                        (first key-val)
                                        'string
                                        "config keys must be strings")
                                      ,hash)
                             ,(second key-val))
                      config-setters)
              :finally (return (nreverse config-setters)))
         ,config))))

@export
(defvar *active-config* (make-config))
