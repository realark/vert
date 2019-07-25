(in-package :cl-user)
(require :split-sequence)

(defpackage :recurse.vert/test
  (:use :cl :recurse.vert :prove)
  (:export :run-prove-tests)
  (:documentation "Vert tests"))
