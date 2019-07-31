;;;; utility.lisp

(in-package :woo)

(defmacro if-do (test consequence alternative &key unconditional)
  "Does UNCONDITIONAL form on both branches."
  `(if ,test
       (progn ,unconditional ,consequence)
       (progn ,unconditional ,alternative)))

(defmacro when-do (test consequence &key unconditional)
  `(if-do ,test ,consequence nil :unconditional ,unconditional))

(defmacro unless-do (test alternative &key unconditional)
  `(if-do ,test nil ,alternative :unconditional ,unconditional))
