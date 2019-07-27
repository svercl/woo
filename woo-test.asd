;;;; woo-test.asd

(defsystem woo-test
  :author "Brad Svercl <bradsvercl@gmail.com>"
  :license "MIT"
  :depends-on (:woo :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "package")
                 (:file "environment")
                 (:file "lexer")
                 (:file "parser"))))
  :perform (test-op (o s) (woo-test::run-all-tests)))
