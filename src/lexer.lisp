;;;; lexer.lisp

(in-package :woo)

;; FIXME: Combine +builtins+ and +tokens+

(defparameter +builtins+
  (alexandria:alist-hash-table
   '(("function" . :function)
     ("elif" . :elif)
     ("else" . :else)
     ("true" . :true)
     ("false" . :false)
     ("let" . :let)
     ("return" . :return))
   :test #'equal))

(defparameter +tokens+
  (alexandria:alist-hash-table
   '((">" . :greater-than)
     (">=" . :greater-equal)
     ("<" . :less-than)
     ("<=" . :less-equal)
     ("=" . :assign)
     ("==" . :equal)
     ("!" . :bang)
     ("!=" . :not-equal)
     ("+" . :plus)
     ("+=" . :plus-equal)
     ("-" . :minus)
     ("-" . :minus-equal)
     ("*" . :star)
     ("*=" . :star-equal)
     ("/" . :slash)
     ("/=" . :slash-equal)
     ("(" . :left-paren)
     (")" . :right-paren)
     ("{" . :left-brace)
     ("}" . :right-brace)
     ("[" . :left-bracket)
     ("]" . :right-bracket)
     ("%" . :percent)
     ("^" . :caret)
     ("~" . :tilde)
     ("||" . :logical-or)
     ("&&" . :logical-and)
     (";" . :semicolon)
     ("@" . :at)
     ("#" . :hash)
     ("'" . :quote)
     (":" . :colon))
   :test #'equal))

(defclass lexer ()
  ((text :reader lexer-text
         :initarg :text
         :type string
         :documentation "The input text.")
   (position :accessor lexer-position
             :initform 0
             :type integer
             :documentation "Where we are right now.")
   (read-position :accessor lexer-read-position
                  :initform 0
                  :type integer
                  :documentation "Next to where we are right now.")
   (current :accessor lexer-current
            :initform nil
            :type (or null character)
            :documentation "The character we are sitting on."))
  (:documentation "Transforms text into tokens."))

(defmethod print-object ((lexer lexer) stream)
  (print-unreadable-object (lexer stream :type t)
    (print (lexer-current lexer) stream)))

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

(defmethod collect-while ((lexer lexer) pred &key no-start)
  "Advance and collect the current character while PRED holds."
  (iterate:iter
    (iterate:with start-position = (lexer-position lexer))
    (iterate:for current-position = (lexer-position lexer))
    (iterate:for current = (lexer-current lexer))
    (iterate:while (or (null current)
                       (funcall pred current)))
    (advance lexer)
    (iterate:finally (return (or no-start
                                 (subseq (lexer-text lexer) start-position current-position))))))

(defmethod skip-whitespace ((lexer lexer))
  (collect-while lexer #'serapeum:whitespacep :no-start t))

(defmethod next-token ((lexer lexer))
  (flet ((read-identifier ()
           (flet ((valid (char)
                    (or (alphanumericp char)
                        (member char '(#\+ #\- #\_ #\!)))))
             (collect-while lexer #'valid)))
         (read-integer ()
           (collect-while lexer #'digit-char-p))
         (read-string ()
           (advance lexer) ; #\"
           (flet ((valid (char)
                    (char/= char #\")))
             (collect-while lexer #'valid)))
         (lookup-identifier (identifier)
           (gethash identifier +builtins+ :identifier))
         (token (kind literal &optional (eat t))
           "Return a token of KIND and LITERAL, maybe advance."
           (prog1 (make-token kind literal)
             (when eat
               (advance lexer)))))
    (skip-whitespace lexer)
    (let* ((current (string (lexer-current lexer)))
           (current-hash (gethash current +tokens+))
           (peeked (string (peek lexer)))
           (combined (concatenate 'string current peeked))
           (combined-hash (gethash combined +tokens+)))
      (cond ((serapeum:true combined-hash)
             (prog1 (token combined-hash combined)
               (advance lexer)))
            ((serapeum:true current-hash)
             (token current-hash current))
            ((string= current #\")
             (token :string (read-string)))
            ((every #'digit-char-p current)
             (token :integer (read-integer) nil))
            ((every #'alphanumericp current)
             (let ((ident (read-identifier)))
               (token (lookup-identifier ident) ident nil)))
            (t (token :eof "eof"))))))
