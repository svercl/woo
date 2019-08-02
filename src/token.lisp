;;;; token.lisp

(in-package :woo)

(defclass token ()
  ((kind :reader token-kind
         :initarg :kind
         :type keyword)
   (literal :reader token-literal
            :initarg :literal
            :type string)
   (line :reader token-line
         :initarg :line
         :initform 0
         :type integer)
   (column :reader token-column
           :initarg :column
           :initform 0
           :type integer)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (with-slots (kind literal) token
      (format stream "Kind: ~S Literal: ~S" kind literal))))

(defun make-token (kind literal)
  (make-instance 'token :kind kind :literal literal))

(defmethod token-precedence ((token token))
  (or (serapeum:assocdr (token-kind token) +token-precedences+) :lowest))

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
