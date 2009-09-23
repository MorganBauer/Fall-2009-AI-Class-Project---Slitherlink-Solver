(in-package #:cl-user)

(defpackage #:slitherlink
  (:use #:common-lisp
        #:it.bese.FiveAM))

(in-package #:slitherlink)

(def-suite slitherlink-tests :description "Main test-suite.")
(in-suite slitherlink-tests)
