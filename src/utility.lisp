;;;; utility.lisp

(in-package :woo)

(defmacro if-do (test consequence alternative &body unconditional)
  "Does UNCONDITIONAL form on both branches."
  `(if ,test
       (progn ,@unconditional ,consequence)
       (progn ,@unconditional ,alternative)))

(defmacro when-do (test consequence &body unconditional)
  `(if-do ,test ,consequence nil ,@unconditional))

(defmacro unless-do (test alternative &body unconditional)
  `(if-do ,test nil ,alternative ,@unconditional))
