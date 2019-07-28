;;;; lexer.lisp

(in-package :woo)

(defparameter +builtins+
  (alist-hash-table
   '(("fn" . :function)
     ("elif" . :elif)
     ("else" . :else)
     ("t" . :t)
     ("nil" . :nil)
     ("let" . :let)
     ("return" . :return)
     ("do" . :do)
     ("end" . :end))
   :test #'equal))

(defparameter +simple-tokens+
  (alist-hash-table
   '((#\+ . :plus)
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
     (#\@ . :at)
     (#\Nul . :eof)))
  "single character tokens")

(defparameter +two-char-tokens+
  (alist-hash-table
   '(("==" . :equal)
     ("!=" . :not-equal)
     ("<=" . :less-equal)
     (">=" . :greater-equal)
     ("&&" . :logical-and)
     ("||" . :logical-or))
   :test #'equal))

(defclass lexer ()
  ((text :reader lexer-text
         :initarg :text
         :type string)
   (position :accessor lexer-position
             :initform 0
             :type integer)
   (read-position :accessor lexer-read-position
                  :initform 0
                  :type integer)
   (current :accessor lexer-current
            :initform nil
            :type (or null character)))
  (:documentation "Transforms text into tokens."))

(defmethod print-object ((lexer lexer) stream)
  (print-unreadable-object (lexer stream)
    (with-slots (current) lexer
      (princ current stream))))

(defun make-lexer (text)
  (make-instance 'lexer :text text))

(defmethod char-at ((lexer lexer) index)
  "Return character at INDEX, if error then the null character."
  (or (ignore-errors (char (lexer-text lexer) index)) #\Nul))

(defmethod peek ((lexer lexer))
  "Return the next character without advancing."
  (char-at lexer (lexer-read-position lexer)))

(defmethod advance ((lexer lexer))
  (with-accessors ((current lexer-current)
                   (position lexer-position)
                   (read-position lexer-read-position))
      lexer
    (setf current (peek lexer)
          position read-position)
    (incf read-position)))

(defmethod collect-while ((lexer lexer) pred)
  "Advance and collect the current character while PRED holds."
  (loop :with start := (lexer-position lexer)
        :for position := (lexer-position lexer)
        :for current := (lexer-current lexer)
        :while (funcall pred current)
        :do (advance lexer)
        :finally (return (subseq (lexer-text lexer) start position))))

(defmethod skip-whitespace ((lexer lexer))
  (loop :for char := (lexer-current lexer)
        :while (or (null char)
                   (whitespacep char))
        :do (advance lexer)))

(defmethod next-token ((lexer lexer))
  (flet ((read-identifier ()
           (flet ((valid (char)
                    (or (alphanumericp char)
                        (member char '(#\- #\_ #\!)))))
             (collect-while lexer #'valid)))
         (read-integer ()
           (collect-while lexer #'digit-char-p))
         (read-string () "")
         (lookup-identifier (ident)
           (gethash ident +builtins+ :identifier))
         (token (kind literal &optional (eat t))
           "Return a token of KIND and LITERAL, maybe advance."
           (prog1 (make-token kind literal)
             (when eat
               (advance lexer)))))
    (skip-whitespace lexer)
    (let* ((current (lexer-current lexer))
           (simple (gethash current +simple-tokens+))
           (both (coerce (list current (peek lexer)) 'string))
           (two-char (gethash both +two-char-tokens+)))
      (cond ((or two-char simple)
             (if two-char
                 (prog1 (token two-char both)
                   (advance lexer))
                 (token simple (string current))))
            ((char= current #\")
             (token :string (read-string)))
            ((digit-char-p current)
             (token :integer (read-integer) nil))
            ((alpha-char-p current)
             (let ((ident (read-identifier)))
               (token (lookup-identifier ident) ident nil)))
            ;; end of file or end of input, whatever.
            (t (token :illegal "illegal"))))))
