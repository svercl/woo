;;;; token.lisp

(in-package :woo)

(defclass token ()
  ((kind :reader token-kind
         :initarg :kind
         :type keyword)
   (literal :reader token-literal
        :initarg :literal
        :type string)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream)
    (format stream "~A: ~S"
            (token-kind token)
            (token-literal token))))

(defun make-token (kind literal)
  (make-instance 'token :kind kind
                        :literal literal))

(defmethod token-precedence ((token token))
  (gethash (token-kind token) *token-precedence* 0))

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
