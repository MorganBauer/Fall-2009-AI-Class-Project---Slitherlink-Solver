(in-package :cl-user)

(defpackage #:slither-asd
  (:use #:common-lisp #:asdf))

(in-package #:slither-asd)

(defsystem #:slither
  :name "Slitherlink"
  :author "Morgan Bauer <mhb@cise.ufl.edu>"
  :author "Dana Preble <d.e.preble@gmail.com>"
  :serial t ;files load in order
  :components ((:file "packages")
               (:file "slither"))
  :depends-on ("fiveam" "split-sequence"))
