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

     #:format-test-label
     #:format-test-results

     ;; test-classes.lisp
     #:test-predicate
     #:test-object
     #:test-setup-function
     #:test-cleanup-function

     #:test

     #:lisp-test
     #:test-lambda-form
     #:test-lambda-function

     #:test-suite
     #:test-suite-default-test-class
     #:register-test-suite
     #:find-test-suite
     #:remove-test-suite     #:map-tests
     #:add-test
     #:remove-test
     
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

