
(in-package :analyzer)

(define-visitor-dispatcher method-call-visitor (node summary)
  (visit (resolve-instance 'method-call-visitor)
         node
         (jobject-lisp-value summary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  METHOD INVOCATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'method-call-visitor)))
                  (decl (jclass +method-call-expr+))
                  summary)
  (let ((scope (#"getScope" decl)))
    (cond
      ((null scope)
       (push (format nil "No scope found for ~A" (stringify decl))
             (summary-messages summary)))
      (t
       (jtypecase scope
         (+name-expr+
          (awhen (get-binding summary (name-of scope))
            (rank-type summary (cdr it))))
         (+object-creation-expr+
          (rank-type summary (stringify (#"getType" scope))))
         (+method-call-expr+
          ;; Again. Static calls (builder/factories) are higher ranked.
          (when (find-parent-type +name-expr+ scope)
            (rank-type summary (stringify scope) 2)))
         (otherwise
          (push (format nil "Don't know how to rank ~A in ~A. Ignoring it"
                        (stringify scope)
                        (stringify decl))
                (summary-messages summary)))))))
  (dojlist (node (#"getChildrenNodes" decl))
    (accept node (resolve-instance 'method-call-visitor) summary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  OBJECT CREATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'method-call-visitor)))
                  (decl (jclass +object-creation-expr+))
                  summary)
  (rank-type summary (stringify (#"getType" decl))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  BINARY EXPRESSION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'method-call-visitor)))
                  (decl (jclass +binary-expr+))
                  summary)
  (let ((left (#"getLeft" decl))
        (right (#"getRight" decl))
        (op (#"getOperator" decl)))
    (when (or (equal op (jfield +binary-expr-operator+ "equals"))
              (equal op (jfield +binary-expr-operator+ "notEquals")))
      (maybe-rank-name-expr left summary visitor)
      (maybe-rank-name-expr right summary visitor))))

(defun maybe-rank-name-expr (node summary visitor)
  (if (superclassp +name-expr+ (#"getClass" node))
      (let ((binding (get-binding summary (#"getName" node))))
        (rank-type summary (cdr binding)))
      (accept node visitor summary)))
