;; test-utils.lisp - utility forms [AFFTA]

(in-package #:info.metacommunity.cltl.test)

(defclass closure-container ()
  ((environment
    :initarg :environment
    :initform nil
    :accessor container-environment)))
