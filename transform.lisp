;;;; transform.lisp
;;;; Transformations on the parse tree.

(in-package :woo)

(defun just-tokens (leaf)
  ;; first one denotes what kind of thing it is,
  ;; e.g., :if-expression :number-literal :prefix-expression
  (case (first leaf)
    ))

(defun transform (tree fun)
  (mapcar fun tree))
