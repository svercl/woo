;;;; woo.lisp

(in-package :woo)

(defclass symbol-table ()
  ((syms :initform (make-hash-table :test #'equal) :reader symtab-symbols)
   (outer :initarg :outer :reader symtab-outer)))

(defun make-symbol-table (&optional outer)
  (make-instance 'symbol-table :outer outer))

(defmethod lookup-symbol ((symtab symbol-table) sym)
  (if-let (outer (symtab-outer symtab))
    (gethash sym (symtab-symbols outer))
    (gethash sym (symtab-symbols symtab))))

(defmethod set-symbol ((symtab symbol-table) sym value)
  (setf (gethash sym (symtab-symbols symtab)) value))

(defun parse-string (text)
  (let* ((lexer (make-lexer text))
         (parser (make-parser lexer))
         (program (parse-program parser)))
    program))

(defun repl ()
  (loop
    (let* ((text (read-line))
           (parsed (parse-string text)))
      (print parsed))))
