;;;; woo.lisp

(in-package :woo)

(defun parse-string (text)
  (let* ((lexer (make-lexer text))
         (parser (make-parser lexer))
         (program (parse-program parser)))
    program))

(defun rpl ()
  (loop (princ ">> ")
        (let* ((text (read-line))
               (parsed (parse-string text)))
          (pprint parsed)
          (terpri))))

(defun repl ()
  (loop (princ ">> ")
        (let* ((text (read-line))
               (parsed (parse-string text))
               (env (make-environment))
               (evaluated (evaluate parsed env)))
          (princ (inspect-object evaluated))
          (terpri))))
