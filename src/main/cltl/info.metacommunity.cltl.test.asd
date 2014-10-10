;; info.metacommunity.cltl.test.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:test-system
    (:use #:asdf #:cl)))

(in-package #:test-system)


(defsystem #:info.metacommunity.cltl.test
  :description 
  "Another Framework for Functional Test Appliation (AFFTA)"
  :version 
  #-AFFTA-1.3 "1.2.1"
  #+AFFTA-1.3 "1.3"
  :homepage "https://github.com/MetaCommunity/affta"
  :license "https://github.com/MetaCommunity/affta/blob/master/LICENSE"
  
  :depends-on (#:info.metacommunity.cltl.utils
               #+AFFTA-1.3 #:info.metacommunity.cltl.application
               )

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

   (:file "test-recording"
          :depends-on 
          ("test-package"
           ))

   (:file "test-protocol"
          :depends-on 
          ("test-classes" 
           "test-recording"
           ))

   #+UNUSED
   (:file "predicates"
          :depends-on ("test-package"))
   ))
 
