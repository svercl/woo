;;;; parser.lisp

(in-package :woo)

(defparameter +precedences+
  '(:lowest :equals :comparison :sum :product :prefix :call))

(deftype precedence ()
  `(member ,@+precedences+))

(defun precedence< (this that)
  (< (position this +precedences+)
     (position that +precedences+)))

(defparameter +token-precedences+
  '((:equal . :equals)
    (:not-equal . :equals)
    (:less-than . :comparison)
    (:less-equal . :comparison)
    (:greater-than . :comparison)
    (:greater-equal . :comarison)
    (:plus . :sum)
    (:minus . :sum)
    (:star . :product)
    (:slash . :product)
    (:left-paren . :call)))

;; NOTE: We can probably handle this specifically, or handle it as above.
(defparameter +infix-kinds+
  '(:plus :minus :star :slash :equal :not-equal :less-than :greater-than :left-paren))

(defclass parser ()
  ((lexer :accessor parser-lexer
          :initarg :lexer
          :type lexer
          :documentation "The token producer.")
   (current :accessor parser-current
            :initarg :current
            :type (or null token))
   (peek :accessor parser-peek
         :initform nil
         :type (or null token))
   (errors :accessor parser-errors
           :initform nil
           :type (or null list)))
  (:documentation "Transforms tokens into an AST."))

(defmethod print-object ((parser parser) stream)
  (print-unreadable-object (parser stream)
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
          peek (next lexer))))

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
      (cerror "Expected ~A but got ~A" kind (peek-kind parser))))

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
        :when statement :collect statement :into program
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
    (let ((name (parse-identifier parser)))
      (expect-peek parser :assign)
      (next parser)
      (let ((value (parse-expression parser)))
        (optional-semicolon parser)
        (list :let-statement token name value)))))

;; "return" EXPRESSION ?";"
(defun parse-return-statement (parser)
  (let ((token (parser-current parser)))
    (next parser) ; skip "return"
    (let ((value (parse-expression parser)))
      (optional-semicolon parser)
      (list :return-statement token value))))

(defun parse-expression-statement (parser)
  (let* ((token (parser-current parser))
         (expr (parse-expression parser)))
    (optional-semicolon parser)
    (list :expression-statement token expr)))

(defun prefix-parser-for (kind)
  ;; implicit nil
  (case kind
    (:identifier #'parse-identifier)
    (:integer #'parse-integer-literal)
    ((:bang :minus) #'parse-prefix-expression)
    ((:t :nil) #'parse-boolean-literal)
    (:left-paren #'parse-grouped-expression)
    (:if #'parse-if-expression)
    (:function #'parse-function-literal)))

(defun parse-expression (parser &optional (precedence :lowest))
  ;; we convert it into an integer inside the loop
  (check-type precedence keyword)
  (loop :with expression = (if-let (prefix (prefix-parser-for (current-kind parser)))
                             (funcall prefix parser)
                             (return nil))
        :for peek-precedence := (peek-precedence parser)
        :while (and (peek-kind/= parser :semicolon)
                    (precedence< precedence peek-precedence))
        ;; if this is an infix expression, we parse it and update expr
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

(defun parse-prefix-expression (parser)
  (let* ((token (parser-current parser))
         (operator (token-literal token)))
    (next parser) ; skip current (operator)
    (let ((right (parse-expression parser :prefix)))
      (list :prefix-expression token operator right))))

(defun parse-boolean-literal (parser)
  (let* ((token (parser-current parser))
         (value (current-kind/= parser :nil)))
    (list :boolean-literal token value)))

(defun parse-grouped-expression (parser)
  (next parser) ; skip (
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
            (alternative nil))
        (when (peek-kind= parser :else)
          (next parser)
          (setf alternative (parse-block-statement parser)))
        (list :if-expression token condition consequence alternative)))))

;; "{" STATEMENT* "}"
(defun parse-block-statement (parser &key (begin-kind :left-brace) (end-kind :right-brace))
  (let ((token (parser-current parser)))
    (expect-peek parser begin-kind)
    (next parser)
    (loop :for not-end-kind := (current-kind/= parser end-kind)
          :for not-eof := (current-kind/= parser :eof)
          :while (and not-end-kind not-eof)
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

(defun parse-infix-expression (parser left)
  (let* ((token (parser-current parser))
         (operator (token-literal token))
         (precedence (token-precedence token)))
    (if (token-kind= token :left-paren)
        (parse-call-expression parser left)
        (progn (next parser)
               (let ((right (parse-expression parser precedence)))
                 (list :infix-expression token operator left right))))))

(defun parse-call-expression (parser left)
  (let* ((token (parser-current parser))
         (arguments (parse-call-arguments parser)))
    (list :call-expression token left arguments)))

(defun parse-call-arguments (parser)
  (when (peek-kind= parser :right-paren)
    (next parser)
    (return-from parse-call-arguments))
  (next parser)
  (loop :with argument := (parse-expression parser)
        :while (peek-kind= parser :comma)
        :do (next parser)
        :do (next parser)
        :collect (parse-expression parser) :into arguments
        :finally (progn (expect-peek parser :right-paren)
                        (return (cons argument arguments)))))
