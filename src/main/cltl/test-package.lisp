;; test-package.lisp - package definition [AFFTA]

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.test
    (:nicknames #:mcicl.test)
    ;; #+CLOSER-MOP
    #+NIL 
    (:shadowing-import-from
     #:c2mop
     #:defmethod
     #:defgeneric
     #:standard-generic-function
     )

    (:use
     #+NIL #:info.metacommunity.cltl.app ;; cf. LABELED-OBJECT, #'NOTIFY
     #:info.metacommunity.cltl.utils
     #:info.metacommunity.cltl.utils.mop
     ;; #+CLOSER-MOP #:c2mop 
     #:bordeaux-threads
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
     #:format-test-results

     ;; test-classes.lisp
     #:test-predicate
     #:test-object
     #:test-predicate
     #:test-setup-function
     #:test-cleanup-function

     #:test

     #:lisp-test
     #:test-lambda-form
     #:test-lambda-function

     #:test-suite
     #:default-goal-set-class
     #:register-test-suite
     #:find-test-suite
     #:remove-test-suite
     #:map-test-suites

     #:add-test
     #:find-test
     #:remove-test
     #:map-tests
     
     ;; test-protocol.lisp
     #:defsuite
     #:deftest
     #:defgoals
     #:run-test-suite

     #:with-test ;; TO DO
     #:run-test  ;; TO DO
     ;; e.g. (Igneous-Math)
     ;;   (with-test (test measurement-magnitude-degree measure-tests-1) (run-test test 1 :|kg|))
     
     #:do-test-setup
     #:do-test-cleanup
     #:do-test

     #:test-reference
     #:test-reference-test

     ;; test-recording.lisp
     #:test-parameters
     #:test-expect-state
     #:test-predicate
     #:test-condition
     #:test-main-values
     #:test-setup-values
     #:test-cleanup-values

     #:test-goal
     #:lisp-test-goal
     #:compute-test-goal-class

     #:test-record
     #:ensure-test-record

     ))

