
(in-package :analyzer)

(define-visitor-dispatcher method-call-visitor (node summary)
  (visit (resolve-instance 'method-call-visitor)
         node
         (jobject-lisp-value summary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  METHOD INVOCATION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod visit ((visitor (eql (resolve-instance 'method-call-visitor)))
                  (decl (jclass +method-call-expr+))
                  summary)
  (let ((scope (scope decl)))
    (cond
      ((null scope)
       (push (format nil "No scope found for ~A" (stringify decl))
             (summary-messages summary)))
      (t
       (jtypecase scope
         (+name-expr+
          (awhen (get-binding summary (name-of scope))
            (rank-type summary (cdr it))))
         (+class-or-interface-type+
          (rank-type summary (name-of scope)))
         (otherwise
          (push (format nil "Don't know how to rank ~A in ~A. Ignoring it"
                        (stringify scope)
                        (stringify decl))
                (summary-messages summary))))))
    (dojlist (node (#"getChildrenNodes" decl))
      ;; try to detect method chains like
      ;;   tt.getUID().getSerial();
      ;; and prevent the double ranking on tt,
      ;; therefore are we comparing the scope 
      (unless (and (string= (string +method-call-expr+)
                            (jclass-of node))
                   (equal scope (scope node)))
        (accept node (resolve-instance 'method-call-visitor) summary)))))


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
        (right (#"getRight" decl)))
    (maybe-rank-name-expr left summary visitor)
    (maybe-rank-name-expr right summary visitor)))

(defun maybe-rank-name-expr (node summary visitor)
  (if (superclassp +name-expr+ (#"getClass" node))
      (let ((binding (get-binding summary (#"getName" node))))
        (rank-type summary (cdr binding)))
      (accept node visitor summary)))
