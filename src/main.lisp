;;;; main.lisp

(in-package :woo)

(defun prompt-read-line (&optional (prompt ">> "))
  (princ prompt)
  (force-output)
  (read-line))

(defun parse-string (text)
  "Take a string and parse it."
  (let* ((lexer (make-lexer text))
         (parser (make-parser lexer))
         (program (parse-program parser)))
    program))

(defun evaluate-string (text)
  (let* ((env (make-environment))
         (program (parse-string text))
         (evaluated (evaluate program env)))
    (princ (inspect-object evaluated))))

(defun rpl ()
  "Read print loop."
  (loop
    (fresh-line)
    (let* ((text (prompt-read-line))
           (program (parse-string text)))
      (pprint program))))

(defun repl ()
  "Read evaluate print loop."
  (let ((env (make-environment)))
    (add-builtins env)
    (loop
      (fresh-line)
      (let* ((text (prompt-read-line))
             (program (parse-string text))
             (evaluated (evaluate program env)))
        (princ (inspect-object evaluated))))))

(defun rep-file (pathname)
  "Read evaluate and print file."
  (let* ((env (make-environment))
         (text (alexandria:read-file-into-string pathname))
         (parsed (parse-string text))
         (evaluated (evaluate parsed env)))
    (princ (inspect-object evaluated))))
