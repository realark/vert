;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:mygame-asd
  (:use :cl :asdf))

(in-package :mygame-asd)

(defsystem mygame
  :name "mygame"
  :version "0.1"
  :author "yourname"
  :components ((:file "packages")
               (:file "src/mygame"))
  :depends-on (#:vert))

(defsystem mygame/test
  :name "mygame/test"
  :description "Tests for mygame"
  :pathname "t/"
  :serial t
  :depends-on (:prove :mygame)
  :components ((:file "packages")
               (:file "mytest"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
