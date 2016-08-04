;;; conditions.lisp

(in-package :analyzer)

(define-condition analyzer-error (error)
  ((raw-excpetion :initarg :raw-exception
                  :reader analyzer-error-raw-exception)))

(define-condition parse-error (analyzer-error)
  ((file :initarg :file
         :accessor parse-error-file)
   (line :initarg :line
         :accessor parse-error-line)
   (column :initarg :column
           :accessor parse-error-column)
   (cause :initarg :cause
          :accessor parse-error-cause))
  (:report (lambda (condition stream)
             (princ (parse-error-cause condition) stream))))

(defun signal-parse-error (file java-exception)
  (let ((cause (#"toString" java-exception)))
    (multiple-value-bind (match registers)
        (ppcre:scan-to-strings "line (\\d+).*column (\\d+)" cause)
      (declare (ignore match))
      (when registers
        (error (make-instance 'parse-error
                              :file file
                              :cause cause
                              :line (aref registers 0)
                              :column (aref registers 1)
                              :raw-exception java-exception))))))

(define-condition security-breach-error (error)
  ((type :accessor type
         :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Possible security breach.~%~
Accessing files or open network connection can be malicious. This
error is signaled because an object of type ~A was detected."
                     (type condition))))
  (:documentation "Condition signaled if possible security breach detected."))
