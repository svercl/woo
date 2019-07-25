;;;; woo.lisp

(in-package :woo)

(defun parse-string (text)
  (let* ((lexer (make-lexer text))
         (parser (make-parser lexer))
         (program (parse-program parser)))
    program))

(defun rpl ()
  (loop (princ ">> ")
        (force-output)
        (let* ((text (read-line))
               (parsed (parse-string text)))
          (pprint parsed)
          (terpri))))

(defun repl ()
  (let ((env (make-environment)))
    (loop (princ ">> ")
          (force-output)
          (let* ((text (read-line))
                 (parsed (parse-string text))
                 (evaluated (evaluate parsed env)))
            (when evaluated
              (princ (inspect-object evaluated)))
            (terpri)))))
