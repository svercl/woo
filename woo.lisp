;;;; woo.lisp

(in-package #:woo)

(defparameter *precedences*
  (dict :lowest 0
        :equals 1
        :less-greater 2
        :sum 3
        :product 4
        :prefix 5
        :call 6))

(defun precedence-number (precedence)
  (gethash precedence *precedences* 0))

(defparameter *token-precedence*
  (dict :equal :lowest
        :not-equal :lowest
        :less-than :less-greater
        :greater-than :less-greater
        :plus :sum
        :minus :sum
        :star :product
        :slash :product
        :left-paren :call))

(defclass parser ()
  ((lexer :initarg :lexer :reader parser-lexer)
   (current :initform nil :reader parser-current)
   (peek :initform nil :reader parser-peek)))

(defmethod print-object ((parser parser) stream)
  (print-unreadable-object (parser stream)
    (format stream "(~A, ~A)"
            (parser-current parser)
            (parser-peek parser))))

(defun make-parser (lexer)
  (let ((parser (make-instance 'parser :lexer lexer)))
    (dotimes (x 2)
      (next parser))
    parser))

(defmethod next ((parser parser))
  (with-slots (lexer current peek) parser
    (setf current peek
          peek (next lexer))))

(defmethod current-kind ((parser parser))
  (token-kind (parser-current parser)))

(defmethod current-kind= ((parser parser) kind)
  (eq (current-kind parser) kind))

(defmethod peek-kind ((parser parser))
  (token-kind (parser-peek parser)))

(defmethod peek-kind= ((parser parser) kind)
  (eq (peek-kind parser) kind))

(defmethod peek-kind/= ((parser parser) kind)
  (not (peek-kind= parser kind)))

(defmethod expect-peek ((parser parser) kind)
  (if (peek-kind= parser kind)
      (next parser)
      (error "Expected ~A but got ~A" kind (peek-kind parser))))

(defmethod current-precedence ((parser parser))
  (token-precedence (parser-current parser)))

(defmethod peek-precedence ((parser parser))
  (token-precedence (parser-peek parser)))

(defmethod optional-semicolon ((parser parser))
  (when (peek-kind= parser :semicolon)
    (next parser)))

(defun parse-program (parser)
  (loop for current = (parser-current parser)
        while current
        for stmt = (parse-statement parser)
        when stmt
          collect stmt into program
        do (next parser)
        finally (return program)))

(defun parse-statement (parser)
  (case (current-kind parser)
    (:let (parse-let-statement parser))
    (:return (parse-return-statement parser))
    (t (parse-expression-statement parser))))

(defun parse-let-statement (parser)
  (let ((current (parser-current parser)))
    (expect-peek parser :identifier)
    (let ((name (parse-identifier parser)))
      (expect-peek parser :assign)
      (next parser)
      (let ((value (parse-expression parser)))
        (optional-semicolon parser)
        (list :let-statement current name value)))))

(defun parse-return-statement (parser)
  (let ((current (parser-current parser)))
    (next parser)
    (let ((value (parse-expression parser)))
      (optional-semicolon parser)
      (list :return-statement current value))))

(defun parse-expression-statement (parser)
  (let* ((current (parser-current parser))
         (expr (parse-expression parser)))
    (optional-semicolon parser)
    (list :expression-statement current expr)))

(defun prefix-parser-for (kind)
  (case kind
    (:identifier #'parse-identifier)))

;; TODO: This doesn't work for :left-paren which needs to call a different function.
(defparameter *infix-kinds*
  '(:plus :minus :star :slash :equal :not-equal :less-than :greater-than))

(defun parse-expression (parser &optional (precedence :lowest))
  (loop with prefix = (prefix-parser-for (current-kind parser))
        with expr = (if prefix
                        (funcall prefix parser)
                        (return nil))
        with precedence-number = (precedence-number precedence)
        for peek-precedence = (peek-precedence parser)
        while (and (peek-kind/= parser :semicolon)
                   (< precedence-number peek-precedence))
        if (member (peek-kind parser) *infix-kinds*)
          do (progn
               (next parser)
               (setf expr (parse-infix-expression parser expr)))
        else do (return expr)
        finally (return expr)))

(defun parse-identifier (parser)
  (let* ((current (parser-current parser))
         (value (token-lit current)))
    (list :identifier current value)))

(defun parse-infix-expression (parser left)
  (let* ((current (parser-current parser))
         (operator (token-lit current))
         (precedence (token-precedence current)))
    (next parser)
    (let ((right (parse-expression precedence)))
      (list :infix-expression current operator left right))))

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
