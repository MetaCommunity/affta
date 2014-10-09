
(in-package #:test)


(defvar *this*)

(defun do-diadic-test (a b expect pred fn)
  (let ((results (multiple-value-list 
                  (funcall fn a b))))
    (signal (cond
              ((funcall pred results expect)
               (find-class 'test-succeeded))
              (t (find-class 'test-failed)))
            :test *this*
            :parameters (list a b)
            :results results)))
               
               
      
