;;;; token.lisp

(in-package :woo)

(defclass token ()
  ((kind :initarg :kind :reader token-kind)
   (lit :initarg :lit :reader token-lit)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream)
    (format stream "~A: ~S" (token-kind token) (token-lit token))))

(defun make-token (kind lit)
  (make-instance 'token :kind kind :lit lit))

(defmethod token-precedence ((token token))
  (precedence-to-integer (gethash (token-kind token) *token-precedence* 0)))

(defmethod token= ((this token) (that token))
  (and (eq (token-kind this)
           (token-kind that))
       (equal (token-lit this)
              (token-lit that))))

(defmethod token/= ((this token) (that token))
  (not (token= this that)))

(defmethod token-kind= ((token token) kind)
  (eq (token-kind token) kind))

(defmethod token-kind/= ((token token) kind)
  (not (token-kind= token kind)))
