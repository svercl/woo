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
  (let ((env (or outer (make-environment))))
    (serapeum:~> text
                 (parse-string)
                 (evaluate _ env)
                 (inspect-object))))

(defun rpl ()
  "Read print loop."
  (loop
    (fresh-line)
    (serapeum:~> (prompt-read-line)
                 (parse-string)
                 (pprint))))

(defun repl ()
  "Read evaluate print loop."
  (let ((env (make-environment)))
    (loop
      (fresh-line)
      (serapeum:~> (prompt-read-line)
                   (evaluate-string _ env)
                   (princ)))))

(defun rep-file (pathname)
  "Read evaluate and print file."
  (serapeum:~> pathname
               (alexandria:read-file-into-string)
               (evaluate-string)
               (princ)))
