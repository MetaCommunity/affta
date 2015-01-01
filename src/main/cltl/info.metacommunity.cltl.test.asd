;; info.metacommunity.cltl.test.asd			-*-lisp-*-

(in-package #:asdf-user)


(defsystem #:info.metacommunity.cltl.test
  :description 
  "Another Framework for Functional Test Appliation (AFFTA)"
  :version "1.3.1"
  :homepage "https://github.com/MetaCommunity/affta"
  :license "https://github.com/MetaCommunity/affta/blob/master/LICENSE"
  
  :depends-on (#:info.metacommunity.cltl.utils
               #+NIL #:info.metacommunity.cltl.app
               #:info.metacommunity.cltl.utils.mop
               #:bordeaux-threads
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
   (:file "test-interface"
          :depends-on 
          ("test-classes"
           ))

   #+UNUSED
   (:file "predicates"
          :depends-on ("test-package"))
   ))
 
