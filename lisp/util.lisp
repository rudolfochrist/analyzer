
(in-package :analyzer)

(defun listify (jlist)
  "Convert java.util.List JLIST to a Lisp list."
  (loop :with iterator = (#"iterator" jlist)
     :while (#"hasNext" iterator)
     :collect (#"next" iterator)))

(defun vectorify (jlist)
  "Converts the java.util.List to a Lisp vector."
  (coerce (listify jlist) 'vector))

(defmacro dojlist ((var jlist) &body body)
  "Iterate a java.util.List JLIST. Each element gets bound to VAR."
  (let ((iterator (gensym)))
    `(unless (null ,jlist)
       (loop :with ,iterator := (#"iterator" ,jlist)
             :while (#"hasNext" ,iterator)
             :do (let ((,var (#"next" ,iterator)))
                   ,@body)))))

(defun stringify (jobject)
  "String representation of the given JOBJECT."
  (unless (null jobject)
   (#"toString" jobject)))

(defun superclassp (class-1 class-2)
  "Checks if CLASS1 is a superclass of CLASS2"
  (when (and class-1 class-2)
    (jclass-superclass-p class-1 class-2)))

(defmacro jtypecase (keyform &body cases)
  "Like TYPECASE but for Java types."
  (let ((gkeyform (gensym "keyform")))
    `(let ((,gkeyform (if ,keyform
                          (#"getClass" ,keyform)
                          +object+)))
       (cond
         ,@(mapcar (lambda (case)
                     (destructuring-bind (type &rest forms) case
                       (if (or (eql type 'otherwise)
                               (eql type t))
                           `(t
                             ,@forms)
                           `((superclassp ,type ,gkeyform)
                             ,@forms))))
                   cases)))))

(defmacro trap-java-exception (form java-exception-name)
  `(handler-case ,form
     (java-exception (condition)
       (unless (string= ,java-exception-name
                        (jclass-of (java-exception-cause condition)))
         (error condition)))))

(defmacro java-package-p (type &rest packages)
  `(when (or ,@(mapcan (lambda (package)
                         (list `(search ,package ,type :test #'string=)
                               `(trap-java-exception
                                 (find-java-class (format nil "~A.~A" ,package ,type))
                                 "java.lang.ClassNotFoundException")))
                       packages))
     t))

(defun java-lang-p (type)
  "Tests if the given type is in java.lang"
  ;; because int and char boxing classes are named differently
  ;; we test them additionally
  (if (or (string= type "int")
          (string= type "char"))
      t
      (java-package-p type "java.lang")))

(defun java-util-p (type)
  "Tests TYPE if is in java.util."
  (when-let ((captures (nth-value 1 (ppcre:scan-to-strings "([\\w.]+)(?:<.*>)?" type))))
    (java-package-p (aref captures 0) "java.util")))

(defun junit-p (type)
  "Test if TYPE is in org.junit or org.hamcrest."
  (java-package-p type "org.junit" "org.hamcrest"))

(defun io-p (type)
  "Test if TYPE is for I/O.

T if TYPE is in java.io, java.nio or java.net"
  (java-package-p type "java.io" "java.nio" "java.net"))

(defun find-jclass (name)
  (let ((arrayp))
    ;; check for arrays
    (multiple-value-bind (start stop)
        (ppcre:scan ".*\\[\\]" name)
      (when (and start stop)
        (setf name (subseq name start (- stop 2)))
        (setf arrayp t)))
    (handler-case (if arrayp
                      (concatenate 'string
                                   (name-of (find-java-class (make-symbol name)))
                                   "[]")
                      (name-of (find-java-class (make-symbol name))))
      (java-exception (condition)
        (declare (ignore condition))
        ;; do nothing here
        )
      (error (condition)
        (let ((args (simple-condition-format-arguments condition)))
          (when (and (stringp (first args))
                     (listp (second args)))
            (if arrayp
                (list (concatenate 'string (first args) "[]")
                      (second args))
                args)))))))

(defun load-jar (path)
  (unless (member path (first (dump-classpath)) :test #'equal)
    (jar-import path)
    (add-to-classpath path)))

(defun version ()
  "Returns the current version number. Format is MAJOR.MINOR.PATCH"
  (asdf:component-version (asdf:find-system :analyzer)))

(declaim (inline string-empty-p))
(defun string-empty-p (string)
  (= 0 (length string)))
