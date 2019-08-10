;;;; evaluator.lisp

(in-package :woo)

(defparameter +true-object+ '(:boolean t))
(defparameter +false-object+ '(:boolean nil))
(defparameter +null-object+ '(:null))

(defun node-kind (node)
  "The first element is always the kind. e.g. (:program statements) (:integer integer)"
  (first node))

(defun node-kind= (kind &rest nodes)
  "Check if ~nodes~ are of ~kind~."
  (iterate:iter
    (iterate:for node in nodes)
    (iterate:always (eq kind (node-kind node)))))

(defun boolean-to-object (test)
  "Converts native ~test~ boolean into an object our evaluator can use."
  (if test +true-object+ +false-object+))

(defun truthyp (node)
  "Everything that is not null or false is true. This does that."
  (trivia:match node
    ((or +null-object+ +false-object+) nil)
    (_ t)))

(defun inspect-object (node)
  "Attempt to stringify ~node~ into something human readable."
  (trivia:match node
    ((or (list :integer value)
         (list :boolean value)
         (list :integer-literal _ value)
         (list :boolean-literal _ value))
     (write-to-string value))
    ((list :identifier _ value) value)
    ((list :return-value value)
     (inspect-object value))
    ((list :array elements)
     (format nil "[~{~A~^, ~}]" (mapcar #'inspect-object elements)))
    ((list :null) "null")
    ((list :function parameters _ body)
     (format nil "function(~{~A~^, ~}) { ~A }"
             (mapcar #'inspect-object parameters)
             (inspect-object body)))
    ((list :string value) value)
    ((list :block-statement _ statements)
     (format nil "~{~A~}" (mapcar #'inspect-object statements)))
    ((list :return-statement _ expression)
     (concatenate 'string "return " (inspect-object expression) ";"))
    (_ (format nil "something else (~A)" (node-kind node)))))

(defun evaluate (node env)
  "Do stuff with ~node~ using the environment ~env~."
  (trivia:match node
    ((list :program statements) (evaluate-program statements env))
    ((list :block-statement _ statements) (evaluate-block-statement statements env))
    ((list :expression-statement _ expression) (evaluate expression env))
    ((list :return-statement _ expression) (evaluate-return-statement expression env))
    ((list :let-statement _ identifier expression) (evaluate-let-statement identifier expression env))
    ((list :integer-literal _ value) (list :integer value))
    ((list :boolean-literal _ value) (list :boolean value))
    ((list :function-literal _ parameters body) (list :function parameters env body))
    ((list :string-literal _ value) (list :string value))
    ((list :array-literal _ elements) (evaluate-array-literal elements env))
    ((list :prefix-expression _ operator right) (evaluate-prefix-expression operator right env))
    ((list :infix-expression _ operator left right) (evaluate-infix-expression operator left right env))
    ((list :if-expression _ condition consequence alternative) (evaluate-if-expression condition consequence alternative env))
    ((list :identifier _ value) (get-from env value))
    ((list :call-expression _ left arguments) (evaluate-call-expression left arguments env))
    ((list :index-expression _ left index) (evaluate-index-expression left index env))
    (_ +null-object+)))

(defun evaluate-program (statements env)
  (iterate:iter
    (iterate:for statement in statements)
    (iterate:for result = (evaluate statement env))
    (when (node-kind= :return-value result)
      (return (second result)))
    (iterate:finally (return result))))

(defun evaluate-block-statement (statements env)
  "Evaluate each statement in ~statements~ until we hit a return-value or error,
if that is so, then we bail and return that."
  (iterate:iter
    (iterate:for statement in statements)
    (iterate:for result = (evaluate statement env))
    (iterate:until (or (node-kind= :return-value result)
                       (node-kind= :error result)))
    (iterate:finally (return result))))

(defun evaluate-return-statement (expression env)
  "Evaluate expression and attach it to a return-value object."
  (alexandria:when-let (value (evaluate expression env))
    (list :return-value value)))

(defun evaluate-let-statement (identifier expression env)
  "Set idenfitier to expression in the env."
  (alexandria:when-let (value (evaluate expression env))
    (let ((name (third identifier)))
      (set-in env name value))))

(defun evaluate-array-literal (elements env)
  (let ((elements (evaluate-expressions elements env)))
    (list :array elements)))

(defun evaluate-prefix-expression (operator right env)
  "operator right where operator is a prefix and right is an expression."
  (let ((right (evaluate right env)))
    (alexandria:switch (operator :test #'equal)
      ("!" (trivia:match right
             ((or +false-object+ +null-object+) +true-object+)
             (_ +false-object+)))
      ("-" (list :integer (- (second right))))
      (t (error "Unknown operator ~A~A" operator right)))))

(defmacro make-infix-operator (operator &optional boolp)
  `(let* ((left-value (second left))
          (right-value (second right))
          (result (funcall ,operator left-value right-value)))
     (if ,boolp (boolean-to-object result) (list :integer result))))

(defmacro make-infix-operator-map (map &key bools)
  (let ((map (mapcar #'alexandria:ensure-list map)))
    `(alexandria:switch (operator :test #'equal)
       ,@(iterate:iter
           (iterate:for (fun &key name) in map)
           (iterate:for stringy = (or name (string fun)))
           ;; FIXME: Insert the result of boolp into the collection, not verbatim.
           (iterate:for boolp = `(member ',fun ,bools))
           (iterate:collect `(,stringy (make-infix-operator ',fun ,boolp))))
       (t (error "Unknown operator ~A ~A ~A" (first left) operator (first right))))))

(defun evaluate-infix-expression (operator left right env)
  "left operator right where left, right are expressions, and
operator is a string designating a function."
  (let ((left (evaluate left env))
        (right (evaluate right env)))
    (cond ((node-kind= :integer left right)
           (make-infix-operator-map (+ - * / < <= > >= (= :name "==") (/= :name "!="))
                                    :bools '(< <= > >= = /=)))
          ((node-kind= :string left right)
           (make-infix-operator-map ((string< :name "<")
                                     (string<= :name "<=")
                                     (string> :name ">")
                                     (string>= :name ">=")
                                     (string= :name "==")
                                     (string/= :name "!="))
                                    :bools '(string< string> string= string/=)))
          (t (alexandria:switch (operator :test #'equal)
               ("==" (boolean-to-object (equal left right)))
               ("!=" (boolean-to-object (not (equal left right))))
               (t (error "Unknown operator ~A ~A ~A"
                         (node-kind left) operator (node-kind right))))))))

(defun evaluate-if-expression (condition consequence alternative env)
  "Conditionally execute consequence or alternative based on truthyness of condition."
  (let ((condition (evaluate condition env)))
    (cond ((truthyp condition) (evaluate consequence env))
          ((serapeum:true alternative) (evaluate alternative env))
          (t +null-object+))))

(defun extend-function-environment (arguments parameters env)
  "Set parameter to argument in env."
  (iterate:iter
    (iterate:with inner = (make-environment env))
    (iterate:for (nil nil parameter-name) in parameters)
    (iterate:for argument in arguments)
    (set-in inner parameter-name argument)
    (iterate:finally (return inner))))

(defun evaluate-call-expression (left arguments env)
  "left(arguments) where left is a function."
  (let ((function (evaluate left env))
        (arguments (evaluate-expressions arguments env)))
    (trivia:match function
      ((list :function parameters env body)
       (let* ((extended-env (extend-function-environment arguments parameters env))
              (result (evaluate body extended-env)))
         (unwrap-return-value result)))
      ((list :builtin lam)
       (funcall lam arguments))
      (_ (error "~A is not a function." (node-kind function))))))

(defun evaluate-index-expression (left index env)
  "left[index] = object at index in left where left is an array, and index is an expression."
  (let ((left (evaluate left env))
        (index (evaluate index env)))
    (trivia:match index
      ((list :index index)
       (trivia:match left
         ((list :array elements)
          (or (nth index elements) +null-object+))
         (t (error "Index operator not supported for ~A"
                   (node-kind left)))))
      (t (error "Not an index ~A" (node-kind index))))))

(defun unwrap-return-value (node)
  "When ~node~ is of kind return-value, get the value from it, otherwise return itself."
  (trivia:match node
    ((list :return-value value) value)
    (_ node)))

(defun evaluate-expressions (expressions env)
  "Evaluate all ~expressions~."
  (mapcar #'(lambda (expression)
              (evaluate expression env))
          expressions))
