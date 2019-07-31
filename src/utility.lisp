;;;; utility.lisp

(in-package :woo)

(defmacro if-do (test consequence alternative &key unconditional)
  `(if ,test
       (progn ,unconditional ,consequence)
       (progn ,unconditional ,alternative)))

(defmacro when-do (test consequence &key unconditional)
  `(if-do ,test ,consequence :unconditional ,unconditional))

(defmacro unless-do (test alternative &key unconditional)
  `(if-do ,test nil ,alternative :unconditional ,unconditional))
