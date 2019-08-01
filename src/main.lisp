;;;; main.lisp

(in-package :woo)

(defun prompt-read-line (&optional (prompt ">> "))
  (princ prompt)
  (force-output)
  (read-line))

(defun parse-string (text)
  "Take a string and parse it."
  (let* ((lexer (make-lexer text))
         (parser (make-parser lexer)))
    (parse-program parser)))

(defun evaluate-string (text &optional outer)
  (let* ((env (or outer (make-environment)))
         (program (parse-string text))
         (evaluated (evaluate program env)))
    (inspect-object evaluated)))

(defun rpl ()
  "Read print loop."
  (loop
    (fresh-line)
    (let* ((line (prompt-read-line))
           (parsed (parse-string line)))
      (pprint parsed))))

(defun repl ()
  "Read evaluate print loop."
  (let ((env (make-environment)))
    (loop
      (fresh-line)
      (let* ((line (prompt-read-line))
             (evaluated (evaluate-string line env)))
        (princ evaluated)))))

(defun rep-file (pathname)
  "Read evaluate and print file."
  (let* ((file (alexandria:read-file-into-string pathname))
         (evaluated (evaluate-string file)))
    (princ evaluated)))
