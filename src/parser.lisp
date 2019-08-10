;;;; parser.lisp

(in-package :woo)

(defparameter +precedences+
  '(:lowest :equals :comparison :sum :product :prefix :call :index))

(defun precedence< (this that)
  (< (position this +precedences+)
     (position that +precedences+)))

(defparameter +token-precedences+
  '((:equal . :equals)
    (:not-equal . :equals)
    (:less-than . :comparison)
    (:less-equal . :comparison)
    (:greater-than . :comparison)
    (:greater-equal . :comparison)
    (:plus . :sum)
    (:minus . :sum)
    (:star . :product)
    (:slash . :product)
    (:left-paren . :call)
    (:left-bracket . :index)))

(defparameter +infix-kinds+
  '(:plus :minus :star :slash
    :equal :not-equal
    :less-than :less-equal :greater-than :greater-equal
    :left-paren :left-bracket))

(defclass parser ()
  ((lexer :accessor parser-lexer
          :initarg :lexer
          :type lexer
          :documentation "The token producer.")
   (current :accessor parser-current
            :initarg :current
            :initform (make-token :eof "eof")
            :type token
            :documentation "The token we are currently sitting on.")
   (peek :accessor parser-peek
         :initform (make-token :eof "eof")
         :type token
         :documentation "The token next to the current.")
   (errors :accessor parser-errors
           :initform nil
           :type (or null list)
           :documentation "The errors we've encountered along the way."))
  (:documentation "Transforms tokens into an AST."))

(defmethod print-object ((parser parser) stream)
  (print-unreadable-object (parser stream :type t)
    (with-slots (lexer current peek) parser
      (format stream "~A Current: ~A Peek: ~A" lexer current peek))))

(defun make-parser (lexer)
  "Create a parser with LEXER."
  (make-instance 'parser :lexer lexer))

(defmethod initialize-instance :after ((parser parser) &key)
  (next parser 2))

(defmethod next ((parser parser) &optional (amount 1))
  "Advance the PARSER."
  (assert (>= amount 1))
  (with-accessors ((lexer parser-lexer)
                   (current parser-current)
                   (peek parser-peek))
      parser
    (dotimes (x amount)
      (setf current peek
            peek (next-token lexer)))))

;;; Shorthand methods.
(defmethod current-kind ((parser parser))
  (token-kind (parser-current parser)))

(defmethod current-kind= ((parser parser) kind)
  (token-kind= (parser-current parser) kind))

(defmethod current-kind/= ((parser parser) kind)
  (not (current-kind= parser kind)))

(defmethod peek-kind ((parser parser))
  (token-kind (parser-peek parser)))

(defmethod peek-kind= ((parser parser) kind)
  (token-kind= (parser-peek parser) kind))

(defmethod peek-kind/= ((parser parser) kind)
  (not (peek-kind= parser kind)))

(defmethod expect-peek ((parser parser) kind)
  "Expect KIND, if so then advance PARSER, otherwise signal an error."
  (if (peek-kind= parser kind)
      (next parser)
      (error "Expected ~A but got ~A" kind (peek-kind parser))))

(defmethod current-precedence ((parser parser))
  (token-precedence (parser-current parser)))

(defmethod peek-precedence ((parser parser))
  (token-precedence (parser-peek parser)))

(defmethod optional-semicolon ((parser parser))
  "Skip semicolon if we're sitting on one."
  (when (peek-kind= parser :semicolon)
    (next parser)))

(defun parse-program (parser)
  (iterate:iter
    (iterate:while (current-kind/= parser :eof))
    (iterate:for statement = (parse-statement parser))
    (iterate:collect statement into statements)
    (next parser)
    (iterate:finally (return (list :program statements)))))

(defun parse-statement (parser)
  (case (current-kind parser)
    (:let (parse-let-statement parser))
    (:return (parse-return-statement parser))
    (t (parse-expression-statement parser))))

;; "let" IDENTIFIER "=" EXPRESSION ?";"
(defun parse-let-statement (parser)
  (let ((token (parser-current parser)))
    (expect-peek parser :identifier)
    (let ((identifier (parse-identifier parser)))
      (expect-peek parser :assign) ; "="
      (next parser)
      (let ((expression (parse-expression parser)))
        (optional-semicolon parser)
        (list :let-statement token identifier expression)))))

;; "return" EXPRESSION ?";"
(defun parse-return-statement (parser)
  (let ((token (parser-current parser)))
    (next parser) ; "return"
    (let ((expression (parse-expression parser)))
      (optional-semicolon parser) ; ?";"
      (list :return-statement token expression))))

(defun parse-expression-statement (parser)
  "Parse an expression disguised as a statement."
  (let* ((token (parser-current parser))
         (expression (parse-expression parser)))
    (optional-semicolon parser) ; ?";"
    (list :expression-statement token expression)))

(defun prefix-parser-for (kind)
  ;; implicit nil
  (case kind
    (:identifier #'parse-identifier)
    (:integer #'parse-integer-literal)
    ((:bang :minus) #'parse-prefix-expression)
    ((:true :false) #'parse-boolean-literal)
    (:string #'parse-string-literal)
    (:left-paren #'parse-grouped-expression)
    (:if #'parse-if-expression)
    (:function #'parse-function-literal)
    (:left-bracket #'parse-array-literal)))

(defun parse-expression (parser &optional (precedence :lowest))
  "The meat of parsing. Decides whether to parse prefix or infix."
  (iterate:iter
    (iterate:with expression = (alexandria:if-let
                                   (prefix (prefix-parser-for (current-kind parser)))
                                 (funcall prefix parser)
                                 (error "Unknown prefix: ~A" (current-kind parser))))
    (iterate:for peek-precedence = (peek-precedence parser))
    (iterate:while (and (peek-kind/= parser :semicolon)
                        (precedence< precedence peek-precedence)))
    (when (member (peek-kind parser) +infix-kinds+)
      (next parser)
      (setf expression (parse-infix-expression parser expression)))
    (iterate:finally (return expression))))

(defmacro with-parser-token ((parser &key literal-name) &body body)
  `(let* ((token (parser-current ,parser))
          (,(or literal-name 'literal) (token-literal token)))
     ,@body))

(defmacro define-simple-literal (name kind &key validator)
  `(defun ,name (parser)
     (with-parser-token (parser)
       (list ,kind token (if ,validator (funcall ,validator literal) literal)))))

(define-simple-literal parse-identifier :identifier)
(define-simple-literal parse-integer-literal :integer-literal
  :validator #'parse-integer)
(define-simple-literal parse-string-literal :string-literal)
(define-simple-literal parse-boolean-literal :boolean-literal
  :validator #'(lambda (literal)
                 (declare (ignore literal))
                 (current-kind/= parser :false)))

(defun parse-prefix-expression (parser)
  (with-parser-token (parser :literal-name operator)
    (next parser)
    (let ((right (parse-expression parser :prefix)))
      (list :prefix-expression token operator right))))

;; "(" EXPRESSION ")"
(defun parse-grouped-expression (parser)
  (next parser) ; "("
  (let ((expr (parse-expression parser)))
    (expect-peek parser :right-paren) ; ")"
    expr))

;; "if" "(" EXPRESSION ")" BLOCK
;; "if" "(" EXPRESSION ")" BLOCK "else" BLOCK
(defun parse-if-expression (parser)
  (let ((token (parser-current parser)))
    (expect-peek parser :left-paren)
    (next parser)
    (let ((condition (parse-expression parser)))
      (expect-peek parser :right-paren)
      (let ((consequence (parse-block-statement parser))
            alternative)
        (when (peek-kind= parser :else)
          (next parser)
          (setf alternative (parse-block-statement parser)))
        (list :if-expression token condition consequence alternative)))))

;; "{" STATEMENT* "}"
(defun parse-block-statement (parser &key (begin-kind :left-brace) (end-kind :right-brace))
  (let ((token (parser-current parser)))
    (expect-peek parser begin-kind)
    (next parser)
    (iterate:iter
      (iterate:for kind = (current-kind parser))
      (iterate:until (member kind `(,end-kind :eof)))
      (iterate:for statement = (parse-statement parser))
      (iterate:collect statement into statements)
      (next parser)
      (iterate:finally (return (list :block-statement token statements))))))

;; "fn" FN-PARAMS BLOCK
(defun parse-function-literal (parser)
  (let* ((token (parser-current parser))
         (parameters (parse-function-parameters parser))
         (body (parse-block-statement parser)))
    (list :function-literal token parameters body)))

;; "(" ")"
;; "(" ( IDENTIFIER ?"," )* ")"
(defun parse-function-parameters (parser)
  (expect-peek parser :left-paren)
  (if (peek-kind= parser :right-paren)
      (progn (next parser) nil)
      (progn (next parser)
             (parse-list-using parser #'parse-identifier :end-kind :right-paren))))

(defun parse-array-literal (parser)
  (let ((token (parser-current parser))
        (elements (parse-expression-list parser :right-bracket)))
    (list :array-literal token elements)))

(defun parse-infix-expression (parser left)
  (let* ((token (parser-current parser))
         (operator (token-literal token))
         (precedence (token-precedence token)))
    (case (token-kind token)
      (:left-paren (parse-call-expression parser left))
      (:left-bracket (parse-index-expression parser left))
      (t (progn (next parser)
                (let ((right (parse-expression parser precedence)))
                  (list :infix-expression token operator left right)))))))

(defun parse-list-using (parser fun &key (end-kind nil end-kind-p) (delimiter-kind :comma))
  (assert end-kind-p)
  (iterate:iter
    (iterate:if-first-time (iterate:collect (funcall fun parser)))
    (iterate:while (peek-kind= parser delimiter-kind))
    (next parser 2)
    (iterate:collect (funcall fun parser))
    (iterate:finally (expect-peek parser end-kind))))

(defun parse-expression-list (parser &optional (end-kind :right-paren) (delimiter-kind :comma))
  (if (peek-kind= parser end-kind)
      (progn (next parser) nil)
      (progn (next parser)
             (parse-list-using parser #'parse-expression :end-kind end-kind
                                                         :delimiter-kind delimiter-kind))))

(defun parse-call-expression (parser left)
  (let* ((token (parser-current parser))
         (arguments (parse-expression-list parser)))
    (list :call-expression token left arguments)))

(defun parse-index-expression (parser left)
  (let ((token (parser-current parser)))
    (next parser)
    (let ((index (parse-expression parser)))
      (expect-peek parser :right-bracket)
      (list :index-expression token left index))))
