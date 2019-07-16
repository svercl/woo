;;;; parser.lisp

(in-package :woo)

(defparameter *precedences*
  (dict :lowest 0
        :equals 1
        :less-greater 2
        :sum 3
        :product 4
        :prefix 5
        :call 6))

(defun precedence-to-integer (precedence)
  (gethash precedence *precedences* 0))

(defmacro precedence-hash-table (alist)
  `(alist-hash-table
    ',(loop :for (name . precedence) :in alist
            :for number := (precedence-to-integer precedence)
            :collect (cons name number))))

(defparameter *token-precedence*
  (precedence-hash-table ((:equal . :lowest)
                          (:not-equal . :lowest)
                          (:less-than . :less-greater)
                          (:greater-than . :less-greater)
                          (:plus . :sum)
                          (:minus . :sum)
                          (:star . :product)
                          (:slash . :product)
                          (:left-paren . :call))))

(defclass parser ()
  ((lexer :initarg :lexer :reader lexer)
   (current :initform nil :reader current)
   (peek :initform nil :reader peek)
   (errors :initform nil :reader errors))
  (:documentation "Transforms tokens into an AST."))

(defmethod print-object ((parser parser) stream)
  (print-unreadable-object (parser stream)
    (format stream "~A~%(~A, ~A)"
            (lexer parser)
            (current parser)
            (peek parser))))

(defun make-parser (lexer)
  "Create a parser using LEXER."
  (let ((parser (make-instance 'parser :lexer lexer)))
    (dotimes (x 2)
      (next parser))
    parser))

(defmethod next ((parser parser))
  "Advance the PARSER."
  (with-slots (lexer current peek) parser
    (setf current peek
          peek (next lexer))))

;;; Shorthand methods.
(defmethod current-kind ((parser parser))
  (token-kind (current parser)))

(defmethod current-kind= ((parser parser) kind)
  (token-kind= (current parser) kind))

(defmethod current-kind/= ((parser parser) kind)
  (not (current-kind= parser kind)))

(defmethod peek-kind ((parser parser))
  (token-kind (peek parser)))

(defmethod peek-kind= ((parser parser) kind)
  (token-kind= (peek parser) kind))

(defmethod peek-kind/= ((parser parser) kind)
  (not (peek-kind= parser kind)))

;; TODO: Collect errors and present them the user, instead of
;; exposing that we use secret alien technology.
(defmethod expect-peek ((parser parser) kind)
  "Expect KIND, if so then advance PARSER, otherwise signal an error."
  (if (peek-kind= parser kind)
      (next parser)
      (error "Expected ~A but got ~A" kind (peek-kind parser))))

(defmethod current-precedence ((parser parser))
  (token-precedence (current parser)))

(defmethod peek-precedence ((parser parser))
  (token-precedence (peek parser)))

(defmethod optional-semicolon ((parser parser))
  (when (peek-kind= parser :semicolon)
    (next parser)))

(defun parse-program (parser)
  (loop :while (current-kind/= parser :eof)
        :for stmt := (parse-statement parser)
        :when stmt
          :collect stmt :into program
        :do (next parser)
        :finally (return program)))

(defun parse-statement (parser)
  (case (current-kind parser)
    (:let (parse-let-statement parser))
    (:return (parse-return-statement parser))
    (t (parse-expression-statement parser))))

;; "let" IDENTIFIER "=" EXPRESSION ?";"
(defun parse-let-statement (parser)
  (let ((token (current parser)))
    (expect-peek parser :identifier)
    (let ((name (parse-identifier parser)))
      (expect-peek parser :assign)
      (next parser)
      (let ((value (parse-expression parser)))
        (optional-semicolon parser)
        (list :let-statement token name value)))))

;; "return" EXPRESSION ?";"
(defun parse-return-statement (parser)
  (let ((token (current parser)))
    (next parser) ; skip "return"
    (let ((value (parse-expression parser)))
      (optional-semicolon parser)
      (list :return-statement token value))))

(defun parse-expression-statement (parser)
  (let* ((token (current parser))
         (expr (parse-expression parser)))
    (optional-semicolon parser)
    (list :expression-statement token expr)))

(defun prefix-parser-for (kind)
  ;; implicit nil
  (case kind
    (:identifier #'parse-identifier)
    (:number #'parse-number-literal)
    ((:bang :minus) #'parse-prefix-expression)
    ((:t :nil) #'parse-boolean)
    (:left-paren #'parse-grouped-expression)
    (:if #'parse-if-expression)
    (:fn #'parse-fn-literal)))

;; TODO: This doesn't work for :left-paren which needs to call a different function.
;; NOTE: We can probably handle this specifically, or handle it as above.
(defparameter *infix-kinds*
  '(:plus :minus :star :slash :equal :not-equal :less-than :greater-than))

(defun parse-expression (parser &optional (precedence :lowest))
  (loop :with prefix := (prefix-parser-for (current-kind parser))
        :with expr = (if prefix
                         (funcall prefix parser)
                         (return nil))
        ;; make precedence actually useful
        :with precedence-number := (precedence-to-integer precedence)
        :for peek-precedence := (peek-precedence parser)
        :while (and (peek-kind/= parser :semicolon)
                    (< precedence-number peek-precedence))
        :when (find (peek-kind parser) *infix-kinds*)
          :do (next parser)
          :and :do (setf expr (parse-infix-expression parser expr))
        :finally (return expr)))

(defun parse-identifier (parser)
  (let* ((token (current parser))
         (value (token-lit token)))
    (list :identifier token value)))

(defun parse-number-literal (parser)
  (let* ((token (current parser))
         (value (parse-integer (token-lit token) :junk-allowed t)))
    (list :number-literal token value)))

(defun parse-prefix-expression (parser)
  (let* ((token (current parser))
         (operator (token-lit token)))
    (next parser) ; skip current (operator)
    (let ((right (parse-expression parser :prefix)))
      (list :prefix-expression token operator right))))

(defun parse-boolean (parser)
  (let* ((token (current parser))
         (value (current-kind/= parser :nil)))
    (list :boolean token value)))

(defun parse-grouped-expression (parser)
  (next parser) ; skip (
  (let ((expr (parse-expression parser)))
    (expect-peek parser :right-paren)
    expr))

;; "if" "(" EXPRESSION ")" BLOCK
;; "if" "(" EXPRESSION ")" BLOCK "else" BLOCK
(defun parse-if-expression (parser)
  (let ((token (current parser)))
    (expect-peek parser :left-paren)
    (next parser)
    (let ((test (parse-expression parser)))
      (expect-peek parser :right-paren)
      (let ((conseq (parse-block-statement parser))
            (alt))
        (when (peek-kind= parser :else)
          (next parser) ; skip "else"
          (setf alt (parse-block-statement parser)))
        (list :if-expression token test conseq alt)))))

;; "{" STATEMENT* "}"
(defun parse-block-statement (parser)
  (let ((token (current parser)))
    (expect-peek parser :left-brace)
    (next parser)
    (loop :while (and (current-kind/= parser :right-brace)
                      (current-kind/= parser :eof))
          :for stmt := (parse-statement parser)
          :when stmt
            :collect stmt :into stmts
            :and :do (next parser)
          :finally (return (list :block token stmts)))))

;; "fn" FN-PARAMS BLOCK
(defun parse-fn-literal (parser)
  (let* ((token (current parser))
         (parameters (parse-fn-parameters parser))
         (body (parse-block-statement parser)))
    (list :fn-literal token parameters body)))

;; "(" ")"
;; "(" ( IDENTIFIER ?"," )* ")"
(defun parse-fn-parameters (parser)
  (expect-peek parser :left-paren)
  ;; no parameters
  (when (peek-kind= parser :right-paren)
    (next parser)
    (return-from parse-fn-parameters nil))
  (next parser)
  (loop :with identifier := (parse-identifier parser)
        :while (peek-kind= parser :comma)
        :do (next parser)
        :do (next parser)
        :collect (parse-identifier parser) :into identifiers
        :finally (progn
                   (expect-peek parser :right-paren)
                   (return (cons identifier identifiers)))))

(defun parse-infix-expression (parser left)
  (let* ((token (current parser))
         (operator (token-lit token))
         (precedence (token-precedence token)))
    (next parser)
    (let ((right (parse-expression parser precedence)))
      (list :infix-expression token operator left right))))
