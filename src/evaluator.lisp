;;;; evaluator.lisp

(in-package :woo)

(defparameter +true-object+ '(:boolean t))
(defparameter +false-object+ '(:boolean nil))
(defparameter +null-object+ '(:null))

(defun node-kind (node)
  (first node))

(defun inspect-object (node)
  (trivia:match node
    ((or (list :integer value)
         (list :boolean value))
     (write-to-string value))
    ((list :null) "null")
    ((list :return-value value)
     (inspect-object value))
    ((list :function _ _ _) "function")))

#+nil
(defun evaluate (node env)
  (case (node-kind node)
    (:program (evaluate-program node env))
    (:block-statement (evaluate-block-statement node env))
    (:expression-statement (evaluate (third node) env))
    (:return-statement (evaluate-return-statement node env))
    (:let-statement (evaluate-let-statement node env))
    (:integer-literal (list :integer (third node)))
    (:boolean-literal (list :boolean (third node)))
    (:prefix-expression (evaluate-prefix-expression node env))
    (:infix-expression (evaluate-infix-expression node env))
    (:if-expression (evaluate-if-expression node env))
    (:identifier (evaluate-identifier node env))
    (:function-literal (evaluate-function-literal node env))
    (:call-expression (evaluate-call-expression node env))))

(defun evaluate (node env)
  (trivia:match node
    ((list :program statements) (evaluate-program statements env))
    ((list :block-statement _ statements) (evaluate-block-statement statements env))
    ((list :expression-statement _ expression) (evaluate expression env))
    ((list :return-statement _ expression) (evaluate-return-statement expression env))
    ((list :let-statement _ identifier expression) (evaluate-let-statement identifier expression env))
    ((list :integer-literal _ value) (list :integer value))
    ((list :boolean-literal _ value) (list :boolean value))
    ((list :prefix-expression _ operator right) (evaluate-prefix-expression operator right env))
    ((list :infix-expression _ operator left right) (evaluate-infix-expression operator left right env))
    ((list :if-expression _ condition consequence alternative) (evaluate-if-expression condition consequence alternative env))
    ((list :identifier _ value) (evaluate-identifier value env))
    ((list :function-literal _ parameters body) (evaluate-function-literal parameters body env))
    ((list :call-expression _ left arguments) (evaluate-call-expression left arguments env))))

(defun evaluate-program (statements env)
  (loop :for statement :in statements
        :for value := (evaluate statement env)
        :when (node-kind= value :return-value)
          :do (return (second value))
        :finally (return value)))

(defun evaluate-block-statement (statements env)
  (loop :with evaluated-statements := (evaluate statements env)
        :for statement :in evaluated-statements
        :until (or (node-kind= statement :return-value)
                   (node-kind= statement :error))
        :finally (return statement)))

(defun evaluate-return-statement (expression env)
  (when-let (value (evaluate expression env))
    (list :return-value value)))

(defun evaluate-let-statement (identifier expression env)
  (when-let (value (evaluate expression env))
    (let ((name (token-literal (second identifier))))
      (set-in env name value))))

(defun evaluate-prefix-expression (operator right env)
  (let ((right (evaluate right env)))
    (flet ((bang-operator ()
             (trivia:match right
               ((or +false-object+ +null-object+) +true-object+)
               (_ +false-object+)))
           (minus-operator ()
             (list :integer (- (second right)))))
      (switch (operator :test #'equal)
        ("!" (bang-operator))
        ("-" (minus-operator))
        (t (error "Unknown operator ~A~A" operator right))))))

(defun %from-native (test)
  (if test +true-object+ +false-object+))

(defun truthyp (node)
  (trivia:match node
    ((or +null-object+ +false-object+) nil)
    (_ t)))

(defun node-kind= (node kind)
  (eq (node-kind node) kind))

(defun node-kind=2 (left right kind)
  (and (node-kind= left kind)
       (node-kind= right kind)))

(defun %evaluate-integer-infix-expression (operator left right)
  (macrolet ((make-operator (operator &optional bool)
               `(let* ((function (symbol-function ,operator))
                       (left-value (second left))
                       (right-value (second right))
                       (result (funcall function left-value right-value)))
                  (if ,bool (%from-native result) (list :integer result)))))
    (switch (operator :test #'equal)
      ("+" (make-operator '+))
      ("-" (make-operator '-))
      ("*" (make-operator '*))
      ("/" (make-operator '/))
      ("<" (make-operator '< t))
      ("<=" (make-operator '<= t))
      (">" (make-operator '> t))
      ("==" (make-operator '= t))
      ("!=" (make-operator '/= t))
      (t (error "Unknown operator ~A ~A ~A"
                (first left) operator (first right))))))

(defun evaluate-infix-expression (operator left right env)
  (let ((left (evaluate left env))
        (right (evaluate right env)))
    (if (node-kind=2 left right :integer)
        (%evaluate-integer-infix-expression operator left right)
        (switch (operator :test #'equal)
          ("==" (%from-native (equal left right)))
          ("!=" (%from-native (not (equal left right))))
          (t (error "Unknown operator ~A ~A ~A"
                    (node-kind left) operator (node-kind right)))))))

(defun evaluate-if-expression (condition consequence alternative env)
  (let ((condition (evaluate condition env)))
    (cond ((truthyp condition) (evaluate consequence env))
          (alternative (evaluate alternative env))
          (t +null-object+))))

(defun evaluate-identifier (value env)
  (get-from env value))

(defun evaluate-function-literal (parameters body env)
  (let ((parameters (evaluate parameters env)))
    (list :function parameters env body)))

(defun evaluate-call-expression (left arguments env)
  (let ((function (evaluate left env))
        (arguments (%evaluate-expressions arguments env)))
    (%apply-function function arguments)))

(defun %evaluate-expressions (expressions env)
  (loop :for expression :in expressions
        :for evaluated := (evaluate expression env)
        :collect evaluated))

(defun %extend-function-environment (function-env parameters arguments)
  (loop :with env := (make-environment function-env)
        :for argument :in arguments
        :for parameter :in parameters
        :do (set-in env parameter argument)
        :finally (return env)))

(defun %unwrap-return-value (node)
  (if (node-kind= node :return-value)
      (second node)
      node))

(defun %apply-function (function arguments)
  (trivia:match function
    ((list _ parameters env body)
     (let* ((extended-env (%extend-function-environment env parameters arguments))
            (result (evaluate body extended-env)))
       (%unwrap-return-value result)))
    (_ (error "Not a function: ~A" function))))
