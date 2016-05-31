
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

(defun main ()
  (format t "~%~%~%Analyzer v~A~%Type help or h for help.~%"
          (analyzer:version))
  (handler-case (repl)
    (error (condition)
      (format t "An error occurred:~%~%")
      (describe condition)
      (quit))))

(main)

