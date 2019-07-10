;;;; woo.asd

(asdf:defsystem #:woo
  :description "Describe woo here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria"
               "serapeum")
  :components ((:file "package")
               (:file "woo")))
