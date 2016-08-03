
(in-package :analyzer)

(defun walk-ast (node &optional (summary (make-summary)))
  (#"visit" (resolve-instance 'analyzer-visitor) node summary)
  summary)

(defun collect-dependencies (node type summary)
  (#"visit" (resolve-instance 'dependency-identifier) node (list nil type summary)))

(defmacro define-visitor-dispatcher (name lambda-list &body body)
  `(defparameter ,(intern (format nil "*~A-CLASS-INSTANCE*" name))
     (jinterface-implementation
      +analyzer-visitor+
      "visit"
      (lambda ,lambda-list
        ,@body))))

(defun resolve-instance (instance)
  (let ((symb (find-symbol (format nil "*~A-CLASS-INSTANCE*" instance) :analyzer)))
    (if (and (not (null symb))
             (boundp symb))
        (symbol-value symb)
        (restart-case (error "No instance ~A found." instance)
          (use-value (value)
            :report "Specify another class."
            :interactive (lambda ()
                           (princ "Value: " *query-io*)
                           (list (read *query-io*)))
            (resolve-instance value))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  PROTOCOL  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric visit (visitor decl summary)
  (:documentation "Processes a given node DECL of the AST."))

(defmethod visit (visitor (decl (jclass +node+)) summary)
  (dojlist (node (#"getChildrenNodes" decl))
    (accept node visitor summary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  WRAPPERS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun name-of (node)
  "Wraps calls to #getName."
  (stringify (#"getName" node)))

(defun parent-node (node)
  "Returns the parent node of the given node NODE."
  (#"getParentNode" node))

(defun find-parent-type (type node)
  (cond
    ((null node) nil)
    ((superclassp type (#"getClass" node))
     node)
    (t
     (find-parent-type type (parent-node node)))))

(defun direct-parent-type-p (type node)
  (superclassp type (#"getClass" (parent-node node))))

(defun accept (node visitor arg)
  (#"accept" node visitor arg))

(defun scope (expr)
  "Determines the scope of the given expression EXPR.

Informally spoken, the scope of a method call is everything before the dot.

    obj.someMethod();
    ^^^

If EXPR starts with a field access (this means a `this`) it gets skipped and the next `NameExpr` is returned. "
  (labels ((recur (scope)
             (when scope
               (jtypecase scope
                 (+name-expr+ scope)
                 (+field-access-expr+
                  (let ((field-scope (#"getScope" scope)))
                    (jtypecase field-scope
                      (+this-expr+
                       (recur (#"getFieldExpr" scope)))
                      (t
                       (recur field-scope)))))
                 (+method-call-expr+
                  (recur (#"getScope" scope)))
                 (+object-creation-expr+
                  (#"getType" scope))
                 (otherwise
                  (error "No matching case for ~A" scope))))))
    (recur expr)))
