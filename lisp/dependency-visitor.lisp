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
    (awhen (and (or (direct-parent-type-p +object-creation-expr+ decl)
                    (direct-parent-type-p +method-call-expr+ decl))
                (type-in-env lexical-env (name-of decl) summary))
      (add-type-dependency summary current-type it))))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +method-call-expr+))
                  args)
  ;; TODO: Return types
  ;; Whenever a method call is used, try to look it's return type
  ;; up. If this is not possible because you're calling a method of the
  ;; CUT [component under test), than check if this method has been used
  ;; previously and has been assigned to a variable for the return type.
  (destructuring-bind (lexical-env current-type summary) args
    (let ((scope (stringify (#"getScope" decl))))
      (when (or (search "assert" (name-of decl) :test #'string-equal)
                (string= current-type scope)
                (string= current-type (type-in-env lexical-env scope summary)))
        (dojlist (arg (#"getArgs" decl))
          (accept arg visitor args))))))

(defmethod visit ((visitor (eql (resolve-instance 'dependency-identifier)))
                  (decl (jclass +object-creation-expr+))
                  args)
  (destructuring-bind (lexical-env current-type summary) args
    (declare (ignore lexical-env))
    (let ((type (stringify (#"getType" decl)))
          (parameters (#"getArgs" decl)))
      (break "current type: ~A~%type: ~A~%parameters: ~A" current-type type parameters)
      ;; FIXME
      ;; WHAT??? Why do I rank the types if the don't match? This doesn't make
      ;; sense at all. Actually, we don't rank a type if it doesnt match.
      (if (string= current-type type)
          (unless (null parameters)
            (dojlist (arg parameters)
              (accept arg visitor args)))
          ;; this has to be removed.
          ;; Sat current type is Bar, ant decl is
          ;;     new Foo();
          ;; Then Foo is not a dependency of Bar, obviously.
          ;; So proceed without doing anything. But if decl is
          ;;     new Bar();
          ;; then we need to process the args of decl (if any).
          (add-type-dependency summary current-type type)))))
