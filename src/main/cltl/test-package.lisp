;; test-package.lisp - package definition [AFFTA]

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.test
    (:nicknames #:test)
    (:shadowing-import-from
     #:c2mop
     #:defmethod
     #:defgeneric
     #:standard-generic-function
     )

    (:use
     #:info.metacommunity.cltl.utils
     #:c2mop #:cl)

    (:export ;; FIXME: Update

     #:test-condition
     #:test-condition-test

     #:test-failed
     #:test
     ))

