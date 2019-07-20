;;;; symbol-table.lisp

(in-package :woo-test)

(test lookup-symbols
  (let* ((inner (woo::make-symbol-table))
         (outer (woo::make-symbol-table inner)))
    (woo::set-symbol inner "inner" "inside")
    (woo::set-symbol outer "outer" "outside")
    (is (null (woo::lookup-symbol inner "outside")))
    (is (equal "inside" (woo::lookup-symbol outer "inner")))))
