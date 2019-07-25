;;;; transform.lisp
;;;; Transformations on the parse tree.

(in-package :woo)

;; NOTE: not the actual values contained
;; (:let-statement token identifier expression)
;; (:return-statement token (:value expression))
;; (:expression-statement token expression)
;; (:identifier token string)
;; (:integer-literal token integer)
;; (:prefix-expression token (:operator string) (:right expression))
;; (:boolean token (:value (not nil)))
;; (:if-expression token (:condition expression) (:consequence block) (:alternative (or nil block)))
;; (:block token (:statements (list statements)))
;; (:fn-literal token (:parameters (list expressions)) (:alternative (or nil block)))
;; (:infix-expression token (:operator string) (:left expression) (:right expression))

(defun transform (tree fun)
  (mapcar fun tree))
