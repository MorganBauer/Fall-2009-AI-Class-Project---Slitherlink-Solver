(in-package #:cl-user)

(defpackage #:slither
  (:use #:common-lisp
        #:split-sequence
        #:lisp-unit)
  (:export slither test-all))

(in-package #:slither)
