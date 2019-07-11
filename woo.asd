;;;; woo.asd

(asdf:defsystem :woo
  :description "A programming language."
  :author "Brad Svercl <bradsvercl@gmail.com"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria"
               "serapeum")
  :components ((:file "package")
               (:file "woo")
               (:file "lexer")
               (:file "token")
               (:file "parser")))
