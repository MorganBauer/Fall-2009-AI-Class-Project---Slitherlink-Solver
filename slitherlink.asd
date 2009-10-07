(in-package :cl-user)

(defpackage #:slitherlink-asd
  (:use #:common-lisp #:asdf))

(in-package #:slitherlink-asd)

(defsystem #:slitherlink
  :name "Slitherlink"
  :author "Morgan Bauer <mhb@cise.ufl.edu>"
  :author "Dana Preble <d.e.preble@gmail.com>"
  :serial t ;files load in order
  :components ((:file "packages")
               (:file "slitherlink"))
  :depends-on ("fiveam" "split-sequence"))
