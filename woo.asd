;;;; woo.asd

(asdf:defsystem :woo
  :description "A programming language."
  :author "Brad Svercl <bradsvercl@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria" "serapeum")
  :components ((:file "package")
               (:file "woo")
               (:file "lexer")
               (:file "token")
               (:file "parser")))

(asdf:defsystem :woo/tests
  :description "Tests for woo"
  :author "Brad Svercl <bradsvercl@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on ("woo" "fiveam")
  :pathname "t/"
  :components ((:file "package")
               (:file "symbol-table")
               (:file "lexer")
               (:file "parser")))
