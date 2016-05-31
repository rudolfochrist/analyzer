
(in-package :analyzer.parser)

(defvar +java-parser+       '|com.github.javaparser.JavaParser|)
(defvar +file-input-stream+ '|java.io.FileInputStream|)
(defvar +string-reader+     '|java.io.StringReader|)

(defun call-with-file-input-stream (path thunk)
  (let ((string-path (restart-case (namestring path)
                       (use-value (value)
                         :report "Different pathname"
                         :interactive (lambda ()
                                        (princ "Pathname: " *query-io*)
                                        (list (read *query-io*)))
                         value))))
    (let ((file-input-stream (new +file-input-stream+ string-path)))
      (unwind-protect
           (funcall thunk file-input-stream)
        (#"close" file-input-stream)))))

(defmacro with-file-input-stream ((file path) &body body)
  `(call-with-file-input-stream ,path #'(lambda (,file) ,@body)))

(defun parse (file)
  (with-file-input-stream (stream file)
    (#"parse" +java-parser+ stream)))

(defun parse-raw (data)
  "Parse DATA without processing in upfront."
  (#"parse" +java-parser+ data java:+false+))

(defun parse-string (string)
  "Creates a compilation unit from the given string STRING"
  (parse-raw (new +string-reader+ string)))

(defun parser-string (object)
  "Returns the string representation of an parser object like COMPILATION-UNIT."
  (if (null object)
      ""
      (#"toString" object)))

(defun parser-print (object &optional stream)
  (print (parser-string object) stream))
