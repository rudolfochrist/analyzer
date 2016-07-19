
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
    `(loop :with ,iterator := (#"iterator" ,jlist)
        :while (#"hasNext" ,iterator)
        :do (let ((,var (#"next" ,iterator)))
              ,@body))))

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
    `(let ((,gkeyform (#"getClass" ,keyform)))
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

(defun java-lang-p (type)
  "Tests if the given type is in java.lang"
  (when (or (search "java.lang." type :test #'string=)
            (trap-java-exception (find-java-class (concatenate 'string "java.lang." type))
                                 "java.lang.ClassNotFoundException"))
    t))

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
