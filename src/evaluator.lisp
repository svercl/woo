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
    ((list :return-value value)
     (inspect-object value))
    ((list :array elements)
     (format nil "[~{~A~^, ~}]" (mapcar #'inspect-object elements)))
    ((list :null) "null")
    ((list :function _ _ _) "<function>")))

(defun evaluate (node env)
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
    ((list :identifier _ value) (evaluate-identifier value env))
    ((list :call-expression _ left arguments) (evaluate-call-expression left arguments env))
    ((list :index-expression _ left index) (evaluate-index-expression left index env))))

(defun evaluate-program (statements env)
  (loop :for statement :in statements
        :for result := (evaluate statement env)
        :when (node-kind= result :return-value)
          :do (return (second result))
        :finally (return result)))

(defun evaluate-block-statement (statements env)
  (loop :for statement :in statements
        :for result := (evaluate statement env)
        :until (or (node-kind= result :return-value)
                   (node-kind= result :error))
        :finally (return result)))

(defun evaluate-return-statement (expression env)
  (alexandria:when-let (value (evaluate expression env))
    (list :return-value value)))

(defun evaluate-let-statement (identifier expression env)
  (alexandria:when-let (value (evaluate expression env))
    (let ((name (third identifier)))
      (set-in env name value))))

(defun evaluate-array-literal (elements env)
  (let ((elements (evaluate-expressions elements env)))
    (list :array elements)))

(defun evaluate-prefix-expression (operator right env)
  (let ((right (evaluate right env)))
    (flet ((bang-operator ()
             (trivia:match right
               ((or +false-object+ +null-object+) +true-object+)
               (_ +false-object+)))
           (minus-operator ()
             (list :integer (- (second right)))))
      (alexandria:switch (operator :test #'equal)
        ("!" (bang-operator))
        ("-" (minus-operator))
        (t (error "Unknown operator ~A~A" operator right))))))

(defun boolean-to-object (test)
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
                  (if ,bool (boolean-to-object result) (list :integer result))))
             (make-operator-map (map)
               `(alexandria:switch (operator :test #'equal)
                  ,@(loop :for (string function boolean) :in map
                          :collect `(,string (make-operator ,function ,boolean)))
                  (t (error "Unknown operator ~A ~A ~A"
                            (first left) operator (first right))))))
    (make-operator-map (("+" '+ nil)
                        ("-" '- nil)
                        ("*" '* nil)
                        ("/" '/ nil)
                        ("<" '< t)
                        ("<=" '<= t)
                        (">" '> t)
                        (">=" '>= t)
                        ("==" '= t)
                        ("!=" '/= t)))))

(defun evaluate-infix-expression (operator left right env)
  (let ((left (evaluate left env))
        (right (evaluate right env)))
    (if (node-kind=2 left right :integer)
        (%evaluate-integer-infix-expression operator left right)
        (alexandria:switch (operator :test #'equal)
          ("==" (boolean-to-object (equal left right)))
          ("!=" (boolean-to-object (not (equal left right))))
          (t (error "Unknown operator ~A ~A ~A"
                    (node-kind left) operator (node-kind right)))))))

(defun evaluate-if-expression (condition consequence alternative env)
  (let ((condition (evaluate condition env)))
    (cond ((truthyp condition) (evaluate consequence env))
          ((listp alternative) (evaluate alternative env))
          (t +null-object+))))

(defun evaluate-identifier (value env)
  (get-from env value))

(defun evaluate-call-expression (left arguments env)
  (let ((function (evaluate left env))
        (arguments (evaluate-expressions arguments env)))
    (trivia:match function
      ((list :function parameters env body)
       (let* ((extended-env (extend-function-environment env parameters arguments))
              (result (evaluate body extended-env)))
         (unwrap-return-value result)))
      ((list :builtin lam)
       (funcall lam arguments))
      (_ (error "Not a function: ~A" function)))))

(defun evaluate-index-expression (left index env)
  (let ((left (evaluate left env))
        (index (evaluate index env)))
    (cond ((and (node-kind= left :array)
                (node-kind= index :integer))
           (let ((index (second index))
                 (elements (second left)))
             (or (nth index elements) +null-object+)))
          (t (error "Index operator not supported for ~A"
                    (node-kind left))))))

(defun extend-function-environment (outer parameters arguments)
  (loop :with inner := (make-environment outer)
        :for parameter :in parameters
        :for argument :in arguments
        :do (set-in inner parameter argument)
        :finally (return inner)))

(defun unwrap-return-value (node)
  (trivia:match node
    ((list :return-value value) value)
    (_ node)))

(defun evaluate-expressions (expressions env)
  (mapcar #'(lambda (expression)
              (evaluate expression env))
          expressions))
