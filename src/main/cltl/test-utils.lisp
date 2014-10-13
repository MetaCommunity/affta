;; test-utils.lisp - utility forms [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defclass closure-container ()
  ((environment
    :initarg :environment
    :initform nil
    :accessor container-environment)))


(defun every= (a b)
  (every #'= a b))

(defun every-eql (a b)
  (every #'eql a b))

(defun every-eq (a b)
  (every #'eq a b))

