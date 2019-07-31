;;;; evaluator.lisp

(in-package :woo-test)

(in-suite woo-evaluator)

(test pemdas
  (macrolet ((check (string expected)
               `(is (equal (woo::evaluate-string ,string)
                           (write-to-string ,expected)))))
    (check "1+1-2" (- (+ 1 1) 2))
    (check "2+10*2/2" (+ 2 (/ (* 10 2) 2)))
    (check "(1+2)*(10-12)" (* (+ 1 2) (- 10 12)))))
