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
            :initform (make-token :illegal "illegal")
            :type token)
   (peek :accessor parser-peek
         :initform (make-token :illegal "illegal")
         :type token)
   (errors :accessor parser-errors
           :initform nil
           :type (or null list)))
  (:documentation "Transforms tokens into an AST."))

(defmethod print-object ((parser parser) stream)
  (print-unreadable-object (parser stream :type t)
    (with-slots (lexer current peek) parser
      (format stream "~A (~A, ~A)" lexer current peek))))

(defun make-parser (lexer)
  "Create a parser using LEXER."
  (make-instance 'parser :lexer lexer))

(defmethod initialize-instance :after ((parser parser) &key)
  (dotimes (x 2)
    (next parser)))

(defmethod next ((parser parser))
  "Advance the PARSER."
  (with-accessors ((lexer parser-lexer)
                   (current parser-current)
                   (peek parser-peek))
      parser
    (setf current peek
          peek (next-token lexer))))

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
  (when (peek-kind= parser :semicolon)
    (next parser)))

(defun parse-program (parser)
  (loop :while (current-kind/= parser :eof)
        :for statement := (parse-statement parser)
        :when statement
          :collect statement :into program
        :do (next parser)
        :finally (return (list :program program))))

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
      (expect-peek parser :assign)
      (next parser)
      (let ((expression (parse-expression parser)))
        (optional-semicolon parser)
        (list :let-statement token identifier expression)))))

;; "return" EXPRESSION ?";"
(defun parse-return-statement (parser)
  (let ((token (parser-current parser)))
    (next parser) ; skip "return"
    (let ((expression (parse-expression parser)))
      (optional-semicolon parser)
      (list :return-statement token expression))))

(defun parse-expression-statement (parser)
  (let* ((token (parser-current parser))
         (expression (parse-expression parser)))
    (optional-semicolon parser)
    (list :expression-statement token expression)))

(defun prefix-parser-for (kind)
  ;; implicit nil
  (case kind
    (:identifier #'parse-identifier)
    (:integer #'parse-integer-literal)
    ((:bang :minus) #'parse-prefix-expression)
    ((:t :nil) #'parse-boolean-literal)
    (:string #'parse-string-literal)
    (:left-paren #'parse-grouped-expression)
    (:if #'parse-if-expression)
    (:function #'parse-function-literal)
    (:left-bracket #'parse-array-literal)))

(defun parse-expression (parser &optional (precedence :lowest))
  (loop :with expression := (alexandria:when-let
                                (prefix (prefix-parser-for (current-kind parser)))
                              (funcall prefix parser))
        :for peek-precedence := (peek-precedence parser)
        :while (and (peek-kind/= parser :semicolon)
                    (precedence< precedence peek-precedence))
        :when (member (peek-kind parser) +infix-kinds+)
          :do (next parser)
          :and :do (setf expression (parse-infix-expression parser expression))
        :finally (return expression)))

(defun parse-identifier (parser)
  (let* ((token (parser-current parser))
         (value (token-literal token)))
    (list :identifier token value)))

(defun parse-integer-literal (parser)
  (let* ((token (parser-current parser))
         (value (parse-integer (token-literal token) :junk-allowed t)))
    (list :integer-literal token value)))

(defun parse-string-literal (parser)
  (let* ((token (parser-current parser))
         (value (token-literal token)))
    (list :string-literal token value)))

(defun parse-prefix-expression (parser)
  (let* ((token (parser-current parser))
         (operator (token-literal token)))
    ;; skip prefix
    (next parser)
    (let ((right (parse-expression parser :prefix)))
      (list :prefix-expression token operator right))))

(defun parse-boolean-literal (parser)
  (let* ((token (parser-current parser))
         (value (current-kind/= parser :nil)))
    (list :boolean-literal token value)))

(defun parse-grouped-expression (parser)
  ;; skip :left-paren
  (next parser)
  (let ((expr (parse-expression parser)))
    (expect-peek parser :right-paren)
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
    ;(next parser)
    (loop :for not-end-kind-p := (current-kind/= parser end-kind)
          :for not-eof-p := (current-kind/= parser :eof)
          :while (and not-end-kind-p not-eof-p)
          :for statement := (parse-statement parser)
          :when statement
            :collect statement :into statements
            :and :do (next parser)
          :finally (return (list :block-statement token statements)))))

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
  ;; no parameters
  (when (peek-kind= parser :right-paren)
    (next parser)
    (return-from parse-function-parameters))
  (next parser)
  (loop :with identifier := (parse-identifier parser)
        :while (peek-kind= parser :comma)
        :do (next parser)
        :do (next parser)
        :collect (parse-identifier parser) :into identifiers
        :finally (progn
                   (expect-peek parser :right-paren)
                   (return (cons identifier identifiers)))))

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

(defun parse-expression-list (parser &optional (end-kind :right-paren))
  (when (peek-kind= parser end-kind)
    (next parser)
    (return-from parse-expression-list))
  (next parser)
  (loop :with expression := (parse-expression parser)
        :while (peek-kind= parser :comma)
        :do (next parser)
        :do (next parser)
        :collect (parse-expression parser) :into expressions
        :finally (progn (expect-peek parser end-kind)
                        (return (cons expression expressions)))))

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
