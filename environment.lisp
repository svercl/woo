;;;; environment.lisp

(in-package :woo)

(defclass environment ()
  ((store :accessor environment-store
          :initform (make-hash-table :test #'equal)
          :type hash-table)
   (outer :reader environment-outer
          :initarg :outer
          :type (or null environment))))

(defun make-environment (&optional outer)
  (make-instance 'environment :outer outer))

(defmethod get-from ((environment environment) name)
  (alexandria:if-let (outer (environment-outer environment))
    (get-from outer name)
    (gethash name (environment-store environment))))

(defmethod set-in ((environment environment) name value)
  (setf (gethash name (environment-store environment)) value))
