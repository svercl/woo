;;;; woo-test.asd

(defsystem "woo-test"
  :author "Brad Svercl <bradsvercl@gmail.com>"
  :license "MIT"
  :depends-on ("woo" "fiveam")
  :components ((:module "t"
                :serial t
                :components
                ((:file "package")
                 (:file "suite")
                 (:file "environment")
                 (:file "lexer")
                 (:file "parser")
                 (:file "evaluator"))))
  :perform (test-op (o c) (symbol-call :woo-test '#:run-all)))
