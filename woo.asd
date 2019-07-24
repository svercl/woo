;;;; woo.asd

(defsystem woo
  :description "A programming language."
  :author "Brad Svercl <bradsvercl@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (:alexandria :serapeum)
  :components ((:file "package")
               (:file "woo")
               (:file "lexer")
               (:file "token")
               (:file "parser")
               (:file "environment")
               (:file "evaluator"))
  :build-operation "program-op"
  :build-pathname "woo"
  :entry-point "woo::repl")
