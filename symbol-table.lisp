;;;; symbol-table.lisp

(in-package :woo)

(defclass symbol-table ()
  ((symbols :accessor symbol-table-symbols
            :initform (make-hash-table :test #'equal)
            :type hash-table)
   (outer :reader symbol-table-outer
          :initarg :outer
          :type (or null symbol-table))))

(defun make-symbol-table (&optional outer)
  (make-instance 'symbol-table :outer outer))

(defmethod lookup-symbol ((symbol-table symbol-table) symbol)
  (if-let (outer (symbol-table-outer symbol-table))
    (lookup-symbol outer symbol)
    (gethash symbol (symbol-table-symbols symbol-table))))

(defmethod set-symbol ((symbol-table symbol-table) symbol value)
  (setf (gethash symbol (symbol-table-symbols symbol-table)) value))
