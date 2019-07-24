;;;; parser.lisp

(in-package :woo)

(defparameter *precedences*
  '(:lowest :equals :comparison :sum :product :prefix :call))

(defun precedence-to-integer (precedence)
  (or (position precedence *precedences*) 0))

;; TODO: This macro doesn't work at compile time apparently.
(defun precedence-hash-table (alist &key (test #'eq))
  (let ((table (loop :for (name . precedence) :in alist
                     :for number := (precedence-to-integer precedence)
                     :collect (cons name number))))
    (alist-hash-table table :test test)))

(defparameter *token-precedence*
  (precedence-hash-table '((:equal . :equals)
                           (:not-equal . :equals)
                           (:less-than . :comparison)
                           (:less-equal . :comparison)
                           (:greater-than . :comparison)
                           (:greater-equal . :comarison)
                           (:plus . :sum)
                           (:minus . :sum)
                           (:star . :product)
                           (:slash . :product)
                           (:left-paren . :call))))

(defclass parser ()
  ((lexer :accessor lexer
          :initarg :lexer
          :type lexer
          :documentation "The token producer.")
   (current :accessor current
            :initarg :current
            :type (or null token))
   (peek :accessor peek
         :initform nil
         :type (or null token))
   (errors :accessor errors
           :initform nil
           :type (or null list)))
  (:documentation "Transforms tokens into an AST."))

(defmethod print-object ((parser parser) stream)
  (print-unreadable-object (parser stream)
    (with-slots (lexer current peek) parser
      (format stream "~A (~A, ~A)" lexer current peek))))

(defun make-parser (lexer)
  "Create a parser using LEXER."
  (let ((parser (make-instance 'parser :lexer lexer)))
    (dotimes (x 2)
      (next parser))
    parser))

(defmethod next ((parser parser))
  "Advance the PARSER."
  (setf (current parser) (peek parser)
        (peek parser) (next (lexer parser))))

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
        :finally (return (list :program program))))

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
    ((:t :nil) #'parse-boolean-literal)
    (:left-paren #'parse-grouped-expression)
    (:if #'parse-if-expression)
    (:function #'parse-function-literal)))

;; TODO: This doesn't work for :left-paren which needs to call a different function.
;; NOTE: We can probably handle this specifically, or handle it as above.
(defparameter *infix-kinds*
  '(:plus :minus :star :slash :equal :not-equal :less-than :greater-than :left-paren))

(defun parse-expression (parser &optional (precedence :lowest))
  (loop :with prefix := (prefix-parser-for (current-kind parser))
        :with expr = (if prefix
                         (funcall prefix parser)
                         ;; TODO: Signal an error
                         (return nil))
        :with precedence-number := (if (keywordp precedence)
                                       (precedence-to-integer precedence)
                                       precedence)
        :for peek-precedence := (peek-precedence parser)
        :for not-semicolon-p := (peek-kind/= parser :semicolon)
        :for lower-precedence-p := (< precedence-number peek-precedence)
        :while (and not-semicolon-p lower-precedence-p)
        :when (find (peek-kind parser) *infix-kinds*)
          :do (next parser)
          :and :do (setf expr (parse-infix-expression parser expr))
        :finally (return expr)))

(defun parse-identifier (parser)
  (let* ((token (current parser))
         (value (token-literal token)))
    (list :identifier token value)))

(defun parse-number-literal (parser)
  (let* ((token (current parser))
         (value (parse-integer (token-literal token) :junk-allowed t)))
    (list :integer-literal token value)))

(defun parse-prefix-expression (parser)
  (let* ((token (current parser))
         (operator (token-literal token)))
    (next parser) ; skip current (operator)
    (let ((right (parse-expression parser :prefix)))
      (list :prefix-expression token operator right))))

(defun parse-boolean-literal (parser)
  (let* ((token (current parser))
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
  (let ((token (current parser)))
    (expect-peek parser :left-paren)
    (next parser)
    (let ((condition (parse-expression parser)))
      (expect-peek parser :right-paren)
      (let ((consequence (parse-block-statement parser))
            (alternative))
        (when (peek-kind= parser :else)
          (next parser) ; skip "else"
          (setf alternative (parse-block-statement parser)))
        (list :if-expression token condition consequence alternative)))))

;; "{" STATEMENT* "}"
(defun parse-block-statement (parser)
  (let ((token (current parser)))
    (expect-peek parser :left-brace)
    (next parser)
    (loop :for not-right-brace := (current-kind/= parser :right-brace)
          :for not-eof := (current-kind/= parser :eof)
          :while (and not-right-brace not-eof)
          :for statement := (parse-statement parser)
          :when statement
            :collect statement :into statements
            :and :do (next parser)
          :finally (return (list :block-statement token statements)))))

;; "fn" FN-PARAMS BLOCK
(defun parse-function-literal (parser)
  (let* ((token (current parser))
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
    (return-from parse-function-parameters nil))
  (next parser)
  (loop :with identifier := (parse-identifier parser)
        :while (peek-kind= parser :comma)
        ;; dotimes?
        :do (next parser)
        :do (next parser)
        :collect (parse-identifier parser) :into identifiers
        :finally (progn
                   (expect-peek parser :right-paren)
                   (return (cons identifier identifiers)))))

(defun parse-infix-expression (parser left)
  (let* ((token (current parser))
         (operator (token-literal token))
         (precedence (token-precedence token)))
    (next parser)
    (let ((right (parse-expression parser precedence)))
      (list :infix-expression token operator left right))))
