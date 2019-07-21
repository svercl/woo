;;;; lexer.lisp

(in-package :woo)

(defparameter *builtins*
  (alist-hash-table '(("fn" . :function)
                      ("elif" . :elif)
                      ("else" . :else)
                      ("t" . :t)
                      ("nil" . :nil)
                      ("let" . :let)
                      ("return" . :return))
                    :test #'equal))

;; TODO: Multi characater tokens like == /= && ||
(defparameter *simple-tokens*
  (alist-hash-table '((#\+ . :plus)
                      (#\- . :minus)
                      (#\* . :star)
                      (#\/ . :slash)
                      (#\= . :assign)
                      (#\! . :bang)
                      (#\< . :less-than)
                      (#\> . :greater-than)
                      (#\( . :left-paren)
                      (#\) . :right-paren)
                      (#\{ . :left-brace)
                      (#\} . :right-brace)
                      (#\[ . :left-bracket)
                      (#\] . :right-bracket)
                      (#\% . :percent)
                      (#\^ . :caret)
                      (#\~ . :tilde)
                      (#\: . :colon)
                      (#\' . :quote)
                      (#\; . :semicolon)
                      (#\& . :ampersand)
                      (#\# . :hash)
                      (#\Nul . :eof)))
  "single character tokens")

(defclass lexer ()
  ((text :reader lexer-text
         :initarg :text
         :type string)
   (position :reader lexer-position
             :initform 0
             :type integer)
   (read-position :accessor lexer-read-position
                  :initform 0
                  :type integer)
   (current :reader lexer-current
            :initform nil
            :type (or null character)))
  (:documentation "Transforms text into tokens."))

(defmethod print-object ((lexer lexer) stream)
  (print-unreadable-object (lexer stream)
    (with-slots (text position current) lexer
      (format stream "~A~%(~A,~A)" text position current))))

(defun make-lexer (text)
  (make-instance 'lexer :text text))

(defmethod next ((lexer lexer))
  "Get the next token"
  (labels ((char-at (where)
             "Return the character at WHERE otherwise the null character."
             (handler-case
                 (char (lexer-text lexer) where)
               (error (condition)
                 (declare (ignore condition))
                 #\Nul)))
           (advance ()
             (with-slots (position read-position current) lexer
               (setf current (peek)
                     position read-position)
               (incf read-position)))
           (peek ()
             "Return the character at RPOS without advancing."
             (char-at (lexer-read-position lexer)))
           (read-while (pred)
             "Advance and collect the current character while PRED holds."
             (loop :for char := (lexer-current lexer)
                   :while (funcall pred char)
                   :do (advance)
                   :collect char :into chars
                   :finally (return (coerce chars 'string))))
           (read-identifier ()
             (flet ((valid (char)
                      (or (alphanumericp char)
                          (char= char #\_))))
               (read-while #'valid)))
           ;; TODO: This doesn't handle hex or anything else.
           (read-number ()
             (read-while #'digit-char-p))
           ;; TODO: This should have a better name.
           (lookup-identifier (ident)
             (gethash ident *builtins* :identifier))
           ;; THIS MUST be the last form.
           (token (kind lit &optional (eat t))
             "Return a token of KIND and LIT, and maybe EAT (advance)."
             (prog1 (make-token kind lit)
               (when eat
                 (advance)))))
    ;; skip over whitespace
    (loop :for char := (lexer-current lexer)
          :while (or (null char) ; only initially
                     (whitespacep char))
          :do (advance))
    (let ((current (lexer-current lexer)))
      ;; simple tokens first
      (when-let (kind (gethash current *simple-tokens*))
        ;; get me outta here!
        (return-from next
          (token kind (string current))))
      (cond ((digit-char-p current)
             (token :number (read-number) nil))
            ((alpha-char-p current)
             (let ((ident (read-identifier)))
               (token (lookup-identifier ident) ident nil)))
            ;; end-of-file or end-of-input, whatever
            (t (token :illegal "illegal"))))))
