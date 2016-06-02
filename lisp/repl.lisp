;;; -*- mode: Lisp; common-lisp-style: modern; slime-coding: utf-8-unix -*-
;;;
;;; repl.lisp

(defparameter *stdout* *standard-output*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *standard-output* (make-broadcast-stream)))

(require 'asdf)
(require 'abcl-contrib)
(require 'quicklisp-abcl)
(pushnew *default-pathname-defaults* asdf:*central-registry*)
(ql:quickload :analyzer)

(defvar *commands*
  '(("(h)elp" "Print available commands")
    ("(q)uit" "Exit the program")
    ("(a)nalyze PATH [json|xml] [OUTPUT]" "Analyze the test specification located at PATH.
Specify with json or xml the output format.")))

;; inspired by https://github.com/mrkkrp/unix-opts/blob/master/unix-opts.lisp#L329
(defun pad-string (string &key (padding 0) newline)
  (let ((str (if newline
                 (concatenate 'string (string #\Newline) string)
                 string)))
    (with-output-to-string (s)
      (loop for char across str
         do (princ char s)
         when (char= char #\Newline)
         do (loop repeat padding
               do (princ #\Space s))))))

(defun print-help (commands)
  (format t "Available commands:~%~%")
  (loop for (cmd description) in commands
     do (format t "~25A  ~A~%"
                cmd
                (pad-string description
                            :padding 27
                            :newline (> (length cmd) 25)))))

(defun repl (&optional interactive)
  "Runs the REPL.
If INTERACTIVE is non-nil the quit command doesn't end the whole process.
This is useful if you're testing things in your LISP REPL."
  (princ "> ")
  (force-output)
  (alexandria:destructuring-case (list (read))
    (((h help))
     (print-help *commands*))
    (((q quit))
     (if interactive
       (return-from repl)
       (quit)))
    (((a analyze) path &optional format output)
     (declare (ignore format output))
     (analyzer:analyze path))
    ((t)
     (princ "unknown command")))
  (terpri)
  (repl interactive))

(opts:define-opts
  (:name
   :help
   :description "Prints this help"
   :short #\h
   :long "help")
  (:name
   :interactive
   :description "Start an interactive REPL session"
   :short #\i
   :long "interactive")
  (:name
   :json
   :description "Set report format to JSON"
   :long "json")
  (:name
   :xml
   :description "Set report format to XML"
   :long "xml"))

(defmacro when-option ((option &key exit) &body body)
  `(let ((option (getf options ,option)))
     (when option
       ,@body
       (when ,exit
         (quit)))))

(defun main ()
  (let ((*standard-output* *stdout*))
    (terpri)
    (handler-case
        (multiple-value-bind (options args)
            (opts:get-opts extensions:*command-line-argument-list*)
          (when-option (:help :exit t)
                       (opts:describe
                        :prefix "analyzer - static analysis of test specifications for test-driven search"
                        :usage-of "analyzer"
                        :args "FILE"))
          (when-option (:interactive)
                       (format t "Type h or help for list of available commands~%") 
                       (repl))
          (let ((summary (analyzer:analyze (first args))))
            (or (when-option (:json)
                             (analyzer:report summary :json t))
                (when-option (:xml)
                             (analyzer:report summary :xml t))
                (analyzer:report summary)))) 
      (error (condition)
        (format t "An error occurred: ~%~%")
        (describe condition)
        (quit))))
  (quit))

(main)

