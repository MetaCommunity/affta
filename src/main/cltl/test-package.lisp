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

    (:export

     ;; test-reporting.lisp
     #:test-condition
     #:test-condition-test
     #:test-result
     #:test-result-record
     #:test-failed
     #:test-succeeded

     #:format-test-label
     #:format-test-results

     ;; test-classes.lisp
     #:test-predicate
     #:test-predicate-function
     #:test-name
     #:test-object
     #:test-setup-function
     #:test-cleanup-function

     #:test

     #:lisp-test
     #:test-lambda-form
     #:test-lambda-function

     #:functional-test
     ;; #:functional-setup-test ;;; not yet implemented
     ;; #:class-protocol-test ;;; not yet implemented

     ;; test-protocol.lisp
     #:do-test-setup
     #:do-test-cleanup
     #:do-test


     ;; test-recording.lisp
     #:test-utility-test
     #:test-parameters
     #:test-expect-state
     #:test-predicate
     #:test-condition
     #:test-main-values
     #:test-setup-values
     #:test-cleanup-values

     #:test-utility
     #:test-goal
     #:lisp-test-goal
     #:test-record
     #:ensure-test-record

     #:format-goal-shorthand
     ))

