;;;; package.lisp

(defpackage woo
  (:use :cl)
  (:import-from :alexandria :when-let :alist-hash-table)
  (:import-from :serapeum :whitespacep))
