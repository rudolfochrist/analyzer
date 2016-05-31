;;; conditions.lisp

(in-package :analyzer)

(define-condition analyzer-error (error)
  ((raw-excpetion :initarg :raw-exception
                  :reader analyzer-error-raw-exception)))

(define-condition parse-error (analyzer-error)
  ((line :initarg :line
         :accessor parse-error-line)
   (column :initarg :column
           :accessor parse-error-column)
   (cause :initarg :cause
          :accessor parse-error-cause))
  (:report (lambda (condition stream)
             (princ (parse-error-cause condition) stream))))

(defun signal-parse-error (java-exception)
  (let ((cause (#"toString" java-exception)))
    (multiple-value-bind (match registers)
        (ppcre:scan-to-strings "line (\\d+).*column (\\d+)" cause)
      (declare (ignore match))
      (when registers
        (error (make-instance 'parse-error
                              :cause cause
                              :line (aref registers 0)
                              :column (aref registers 1)
                              :raw-exception java-exception))))))
