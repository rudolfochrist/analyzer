
(in-package :analyzer)

(define-visitor-dispatcher analyzer-visitor (node summary)
  (visit (resolve-instance 'analyzer-visitor)
         node
         (jobject-lisp-value summary)))


;;;;;;;;;;;;;;;;;;;;;;;;  CLASS/INTERFACE DECLARATION  ;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +class-or-interface-declaration+)) summary)
  (setf (summary-test-name summary) (#"getName" decl))
  (dojlist (node (#"getChildrenNodes" decl))
    (accept node visitor summary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;  IMPORT DECLARATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +import-declaration+)) summary)
  (add-import summary (name-of decl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FIELD DECLARATIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +field-declaration+)) summary)
  ;; add global bindings. Some of them will probably be overwritten by
  ;; object/array creation
  (let ((type (stringify (#"getType" decl))))
    (with-constant-signature ((get-id "getId")
                              (get-name "getName"))
      (dojlist (var (#"getVariables" decl))
        (add-global-binding summary (get-name (get-id var)) type))))
  ;; process object creation in child nodes
  (dojlist (node (#"getChildrenNodes" decl))
    (accept node visitor summary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  OBJECT CREATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +object-creation-expr+)) summary)
  (let ((type (#"getType" decl)))
    (awhen (or (find-parent-type +variable-declarator+ decl)
               (find-parent-type +assign-expr+ decl))
      (rank-type summary (stringify type)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  METHOD DECLARATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +method-declaration+)) summary)
  (flet ((junit-test-p (method-decl)
           (remove-if-not (lambda (annotation)
                            (string= (stringify annotation) "@Test"))
                          (listify (#"getAnnotations" method-decl)))))
    (when (junit-test-p decl)
      (create-new-local-binding summary (#"getName" decl))
      (dojlist (node (#"getChildrenNodes" decl))
        (accept node visitor summary)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;  METHOD CALL EXPRESSIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +method-call-expr+)) summary)
  (when (search "assert" (#"getName" decl) :test #'string-equal)
    (dojlist (node (#"getArgs" decl))
      (visit (resolve-instance 'method-call-visitor) node summary)))

  ;; FIXME: This is ugly. First checking if parent type and then
  ;; dispatching manually on type. Urgh.
  (awhen (or (find-parent-type +variable-declaration-expr+ decl)
             (find-parent-type +assign-expr+ decl))
    (let ((scope (scope decl)))
      (jtypecase it
        (+variable-declaration-expr+
         (rank-type summary (stringify (#"getType" it))))
        (+assign-expr+
         (rank-type summary (stringify (#"getTarget" it)))))
      (if (null scope)
          (push (format nil "No scope in ~A. Check static imports." (stringify decl))
                (summary-messages summary))
          (let ((string-scope (stringify scope)))
            (rank-type summary (or (cdr (get-local-binding summary string-scope))
                                   (cdr (get-global-binding summary string-scope))
                                   string-scope)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;  VARIABLE DECLARATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'analyzer-visitor))) (decl (jclass +variable-declaration-expr+)) summary)
  #|
  Variable declarations should be added to the local bindings, but they SHOULD NOT
  be ranked, e.g.

  SearchType a = new SearchType();
  SearchType b = a;
  SearchType c = b;

  SearchType gets ranked once for the object creation. When later a method is
  called on on of the other variables, this should indeed increase the ranking of
  the type. Therefore it is important to know these bindings.
  |#
  (let ((type (stringify (#"getType" decl))))
    (with-constant-signature ((get-id "getId")
                              (get-name "getName"))
      (dojlist (var (#"getVars" decl))
        (add-local-binding summary (get-name (get-id var)) type))))
  ;; process children
  (dojlist (child (#"getChildrenNodes" decl))
    (accept child visitor summary)))
