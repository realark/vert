(in-package :cl-user)

(defpackage :mygame
  (:use :cl :recurse.vert))

(defpackage :mygame-test
  (:use :prove :cl :mygame))
