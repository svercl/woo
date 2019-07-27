;;;; package.lisp

(defpackage woo
  (:use :cl)
  (:import-from :alexandria :if-let :when-let :alist-hash-table :switch :assoc-value)
  (:import-from :serapeum :whitespacep))
