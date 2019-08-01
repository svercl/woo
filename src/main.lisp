;;;; main.lisp

(in-package :woo)

(defun prompt-read-line (&optional (prompt ">> "))
  (princ prompt)
  (force-output)
  (read-line))

(defun parse-string (text)
  "Take a string and parse it."
  (serapeum:~> text
               (make-lexer)
               (make-parser)
               (parse-program)))

(defun evaluate-string (text &optional outer)
  (let* ((env (or outer (make-environment)))
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
    (loop
      (fresh-line)
      (let* ((text (prompt-read-line))
             (evaluated (evaluate-string text env)))
        (princ (inspect-object evaluated))))))

(defun rep-file (pathname)
  "Read evaluate and print file."
  (let* ((text (alexandria:read-file-into-string pathname))
         (evaluated (evaluate-string text)))
    (princ (inspect-object evaluated))))
