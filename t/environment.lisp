;;;; environment.lisp

(in-package :woo-test)

(in-suite woo-environment)

(test lookup-symbols
  (let* ((inner (woo::make-environment))
         (outer (woo::make-environment inner)))
    (woo::set-in inner "inner" "inside")
    (woo::set-in outer "outer" "outside")
    (is (null (woo::get-from inner "outside")))
    (is (equal "inside" (woo::get-from outer "inner")))))
