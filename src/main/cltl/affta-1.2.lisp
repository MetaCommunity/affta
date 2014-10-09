(in-package #:test)


(defvar *this*)


(defgeneric do-diadic-test (a b expect pred fn)
  (:method :around (a b expect pred fn)
           (let ((results (multiple-value-list
                           (call-next-method))))
             (signal (cond
                       ((funcall pred results expect)
                        (find-class 'test-succeeded))
                       (t (find-class 'test-failed)))
                     :test *this*
                     :parameters (list a b)
                     :results results)))
  (:method (a b expect pred fn)
    (funcall fn a b)))


(defun do-monadic-test (a expect pred fn)
  (let ((results (multiple-value-list
                  (funcall fn a))))
    (signal (cond
              ((funcall pred results expect)
               (find-class 'test-succeeded))
              (t (find-class 'test-failed)))
            :test *this*
            :parameters (list a)
            :results results)))


(defun do-variadic-test (values expect pred fn)
  (let ((results (multiple-value-list
                  (apply fn values))))
    (signal (cond
              ((funcall pred results expect)
               (find-class 'test-succeeded))
              (t (find-class 'test-failed)))
            :test *this*
            :parameters (list a)
            :results results)))
