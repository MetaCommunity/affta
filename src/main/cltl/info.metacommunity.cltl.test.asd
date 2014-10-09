;; info.metacommunity.cltl.test.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:test-system
    (:use #:asdf #:cl)))

(in-package #:test-system)


(defsystem #:info.metacommunity.cltl.test
  :description 
  "Another Framework for Functional Test Appliation *(AFFTA)"
  :version "1.1"
  ;; :homepage "https://github.com/MetaCommunity/mci-cltl-test"
  ;; :license "https://github.com/MetaCommunity/mci-cltl-test/blob/master/LICENSE"
  
  :depends-on (#:closer-mop
               #:info.metacommunity.cltl.utils)

  :components 
  ((:file "test-package")

   (:file "test-utils"
	  :depends-on 
          ("test-package"))

   (:file "test-reporting"
          :depends-on 
          ("test-package"))

   (:file "test-classes"
          :depends-on 
          ("test-reporting"
           "test-utils"
           ))
   (:file "test-protocol"
          :depends-on 
          ("test-classes" 
           ))
   #+UNUSED
   (:file "predicates"
          :depends-on ("test-package"))
   ))
 
