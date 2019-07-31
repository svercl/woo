;;;; woo.asd

(defsystem "woo"
  :author "Brad Svercl <bradsvercl@gmail.com>"
  :maintainer "Brad Svercl <bradsvercl@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/bsvercl/woo"
  :version "0.1.0"
  :depends-on ("alexandria" "serapeum" "trivia")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "main")
                 (:file "lexer")
                 (:file "token")
                 (:file "parser")
                 (:file "environment")
                 (:file "evaluator")
                 (:file "utility"))))
  :description "A programming language."
  :in-order-to ((test-op (test-op "woo-test")))
  :build-operation "program-op"
  :build-pathname "woo"
  :entry-point "woo::repl")
