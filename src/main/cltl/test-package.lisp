;; test-package.lisp - package definition [AFFTA]

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.test
    (:nicknames #:test)
    ;; #+CLOSER-MOP
    #+NIL 
    (:shadowing-import-from
     #:c2mop
     #:defmethod
     #:defgeneric
     #:standard-generic-function
     )

    (:use
     #:info.metacommunity.cltl.application ;; cf. LABELED-OBJECT, #'NOTIFY
     #:info.metacommunity.cltl.utils
     ;; #+CLOSER-MOP #:c2mop 
     #:cl
     )

    (:export ;; FIXME: Update

     #:test-condition
     #:test-condition-test

     #:test-failed
     #:test
     ))

