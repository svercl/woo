;;;; token.lisp

(in-package :woo)

(defclass token ()
  ((kind :initarg :kind :reader token-kind)
   (lit :initarg :lit :reader token-lit)
   (precedence :initarg :precedence :reader token-precedence)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream)
    (format stream "(~a . ~a)" (token-kind token) (token-lit token))))

(defun make-token (kind lit)
  (let ((precedence (precedence-number (gethash kind *token-precedence* 0))))
    (make-instance 'token :kind kind :lit lit :precedence precedence)))

(defmethod token= ((this token) (that token))
  (and (eq (token-kind this)
           (token-kind that))
       (equal (token-lit this)
              (token-lit that))))

(defmethod token/= ((this token) (that token))
  (not (token= this that)))
