(in-package :analyzer)

(define-visitor-dispatcher dependency-identifier (node args)
  (visit (resolve-instance 'dependency-identifier)
         node
         (jobject-lisp-value args)))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +method-declaration+))
                  args)
  (accept (#"getBody" decl) visitor (cons (name-of decl)
                                          (cdr args))))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +string-literal-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type "java.lang.String")))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +boolean-literal-expression+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type " java.lang.Boolean")))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +char-literal-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type "java.lang.Character")))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +double-literal-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type "java.lang.Double")))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +integer-literal-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type "java.lang.Integer")))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +long-literal-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type "java.lang.Long")))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +cast-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (add-type-dependency summary current-type (stringify (#"getType" decl)))
    (accept (#"getExpr" decl) visitor args)))

(defun type-in-env (env name summary)
  "Returns the type of NAME in environment with denoted by ENV."
  (let ((bindings (merge 'list
                         (copy-alist (summary-global-bindings summary))
                         (copy-alist (cdr (assoc env
                                                 (summary-local-bindings summary)
                                                 :test #'string=)))
                         #'string=
                         :key #'car)))
    (cdr (assoc name bindings :test #'string=))))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +name-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (awhen (type-in-env lexical-env (name-of decl) summary)
      (add-type-dependency summary current-type it))))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +method-call-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore current-type))
    (let ((type (type-in-env lexical-env (stringify (#"getScope" decl)) summary)))
      (dojlist (arg (#"getArgs" decl))
        (accept arg visitor (list lexical-env
                                  type
                                  summary))))))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +object-creation-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore current-type))
    (let ((type (stringify (#"getType" decl))))
      (dojlist (arg (#"getArgs" decl))
        (when (and (superclassp +object-creation-expr+ (jclass-of arg))
                   (string/= type (stringify (#"getType" arg))))
          (add-type-dependency summary type (stringify (#"getType" arg))))
        (accept arg visitor (list lexical-env
                                  type
                                  summary))))))
