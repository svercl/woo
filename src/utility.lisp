;;;; utility.lisp

(in-package :woo)

(defmacro if-do (test consequence alternative &body unconditional)
  "Does ~unconditional~ form on both branches."
  `(if ,test
       (progn ,@unconditional ,consequence)
       (progn ,@unconditional ,alternative)))

(defmacro when-do (test consequence &body unconditional)
  "Does ~unconditional~ form on both branches. Runs consequence on truthy. Returns nil otherwise."
  `(if-do ,test ,consequence nil ,@unconditional))

(defmacro unless-do (test alternative &body unconditional)
  "Does ~unconditional~ form on both branches. Run alternative of falsy. Returns nil otherwise."
  `(if-do ,test nil ,alternative ,@unconditional))
