;;;; woo.lisp

(in-package #:woo)

(defparameter *builtins*
  (alexandria:alist-hash-table
   '(("fn" . :fn)
     ;; control flow
     ("if" . :if)
     ("elif" . :elif)
     ("else" . :else)
     ("do" . :do)
     ;; booleans
     ("t" . :t)
     ("nil" . :nil))
   ;; these are strings
   :test #'equal))

(defparameter *precedences*
  (serapeum:dict :lowest 0
                 :equals 1
                 :less-greater 2
                 :sum 3
                 :product 4
                 :prefix 5
                 :call 6))

(defun precedence-for (precedence)
  (gethash precedence *precedences*))

(defparameter *simple-tokens*
  (serapeum:dict #\+ :plus
                 #\- :minus
                 #\* :star
                 #\/ :slash
                 #\= :assign
                 #\! :bang
                 #\< :less-than
                 #\> :greater-than
                 #\( :left-paren
                 #\) :right-paren
                 #\{ :left-brace
                 #\} :right-brace
                 #\[ :left-bracket
                 #\] :right-bracket
                 #\% :percent
                 #\^ :caret
                 #\~ :tilde
                 #\: :colon
                 #\' :quote
                 #\; :semicolon
                 #\& :ampersand)
  "single character tokens")

(defun make-token (kind lit)
  (check-type kind symbol)
  (check-type lit string)
  (cons kind lit))

(defun token-kind (token)
  (first token))

(defun token-lit (token)
  (second token))

(defun token= (this that)
  (and (eq (token-kind this)
           (token-kind that))
       (string= (token-lit this)
                (token-lit that))))

(defun token/= (this that)
  (not (token= this that)))

;; TODO: Honestly, this should be a struct.
(defclass lexer ()
  ((text :initarg :text :reader lexer-text)
   (pos :initform 0 :reader lexer-pos)
   (rpos :initform 0 :reader lexer-rpos)
   (ch :initform nil :reader lexer-ch)))

;; TODO: Yeah, it really should.
(defun make-lexer (text)
  (make-instance 'lexer :text text))

(defun lexer-next (lexer)
  "Get the next token"
  (labels ((char-at (where)
             "Return the character at RPOS otherwise the null character."
             (handler-case
                 (char (lexer-text lexer) where)
               (error (c)
                 (declare (ignore c))
                 #\Nul)))
           (advance ()
             (with-slots (pos rpos ch) lexer
               (setf ch (char-at rpos)
                     pos rpos)
               (incf rpos)))
           (peek ()
             "Return the character at RPOS without advancing."
             (char-at (lexer-rpos lexer)))
           (read-while (pred)
             "Advance and collect the current character while PRED holds."
             (loop for ch = (lexer-ch lexer)
                   while (funcall pred ch)
                   do (advance)
                   collect ch into chars
                   finally (return (concatenate 'string chars))))
           (read-identifier ()
             (read-while #'alphanumericp))
           (read-number ()
             (read-while #'digit-char-p))
           ;; TODO: This should have a better name.
           (lookup-identifier (ident)
             (gethash ident *builtins* :identifier))
           (token (kind lit &optional (eat t))
             "Return a token of KIND and LIT, and maybe EAT (advance)."
             (prog1 (make-token kind lit)
               (when eat
                 (advance)))))
    ;; skip over whitespace
    (loop while (or (null (lexer-ch lexer))
                    (serapeum:whitespacep (lexer-ch lexer)))
          do (advance))
    (let ((current (lexer-ch lexer)))
      ;; simple tokens first
      (alexandria:when-let (kind (gethash current *simple-tokens*))
        (return-from lexer-next
          (token kind (string current))))
      (cond ((digit-char-p (lexer-ch lexer))
             (token :number (read-number) nil))
            ((alpha-char-p (lexer-ch lexer))
             (let ((ident (read-identifier)))
               (token (lookup-identifier ident) ident nil)))
            (t nil)))))

(defclass parser ()
  ((lexer :initarg :lexer :reader parser-lexer)
   (current :initform nil :reader parser-current)
   (peek :initform nil :reader parser-peek)))

(defun repl ()
  (loop
    (let* ((text (read-line))
           (lexer (make-lexer text)))
      (loop for token = (lexer-next lexer)
            while token
            do (print token)
            finally (fresh-line)))))
