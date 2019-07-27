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
    (with-slots (kind literal) token
      (format stream "~A: ~S" kind literal))))

(defun make-token (kind literal)
  (make-instance 'token :kind kind :literal literal))

(defmethod token-precedence ((token token))
  (or (assoc-value +token-precedences+ (token-kind token)) :lowest))

(defmethod token= ((this token) (that token))
  (and (eq (token-kind this)
           (token-kind that))
       (equal (token-literal this)
              (token-literal that))))

(defmethod token/= ((this token) (that token))
  (not (token= this that)))

(defmethod token-kind= ((token token) kind)
  (eq (token-kind token) kind))

(defmethod token-kind/= ((token token) kind)
  (not (token-kind= token kind)))
