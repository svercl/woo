;;;; package.lisp

(defpackage woo
  (:use :cl)
  (:import-from :alexandria :when-let :alist-hash-table :switch)
  (:import-from :serapeum :whitespacep))
