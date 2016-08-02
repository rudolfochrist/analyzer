
(in-package :analyzer)

(defstruct summary
  imports global-bindings local-bindings types messages
  test-name ambiguous-imports type-dependecies file-path)

(defun add-import (summary import)
  (pushnew import (summary-imports summary) :test #'string=))

(defun get-imports (summary)
  (summary-imports summary))

(defun import-available-p (type summary)
  "Checks if TYPE matches on of the imports."
  (some #'numberp
        (mapcar (lambda (import)
                  (search type import :test #'string=))
                (summary-imports summary))))

(defun possible-search-subjects (summary)
  "Lists the possibly found search subjects, sorted by rank."
  (let ((copy (copy-alist (summary-types summary))))
    (sort copy #'> :key #'cdr)))

(defun rank-type (summary type &optional (rank 1))
  "Adds the given RANK to the type TYPE."
  ;; some types are excluded from ranking:
  ;;   1. everything in java.lang
  ;;   2. everything in java.util
  ;;   3. everything in org.junit and org.hamcrest
  (unless (or (java-lang-p type)
              (java-util-p type)
              (junit-p type))
    (aif (assoc type (summary-types summary) :test #'string=)
         (incf (cdr it) rank)
         (push (cons type rank) (summary-types summary)))))

(defun create-new-local-binding (summary scope-name)
  (push (list scope-name) (summary-local-bindings summary)))

(defun add-local-binding (summary id type)
  (destructuring-bind (scope &rest bindings)
      (first (summary-local-bindings summary))
    (acond
      ((assoc id bindings :test #'string=)
       (unless (string= type (cdr it))
         ;; TODO: This should consider the inheritance chain.
         (error "The id ~A is already of type ~A in scope ~A. Cannot assign value of type ~A."
                id (cdr it) scope type)))
      (t
       (push (cons id type) (cdr (first (summary-local-bindings summary))))))))

(defun add-global-binding (summary id type)
  (aif (assoc id (summary-global-bindings summary) :test #'string=)
       (unless (string= type (cdr it))
         (error "There is already a binding ~A of the type ~A."
                id (cdr it) type))
       (push (cons id type) (summary-global-bindings summary))))

(defun get-binding (summary identifier)
  "Resolves the the binding for IDENTIFIER either locally or globally.
A binding the form (identifier . type)."
  (or (get-local-binding summary identifier)
      (get-global-binding summary identifier)))

(defun get-local-binding (summary identifier)
  (assoc identifier (cdr (first (summary-local-bindings summary))) :test #'string=))

(defun get-global-binding (summary identifier)
  (assoc identifier (summary-global-bindings summary) :test #'string=))

(defun print-messages (summary &optional (stream *standard-output*))
  (aif (summary-messages summary)
       (format stream "Messages:~%~{  ~A~&~}" it)
       (format stream "No messages~&")))

(defun resolve-import (name summary)
  (let ((class (find-jclass name)))
    (unless (null class)
      (etypecase class
        (string
         (add-import summary class))
        (list
         (push class (summary-ambiguous-imports summary)))))))

(defun unresolved-types (summary)
  "Returns a list of types without imports or ambiguous imports."
  (remove-if (lambda (type)
               (or (ambiguous-import-for-type type summary)
                   (import-available-p type summary)))
             (mapcar #'car (summary-types summary))))

(defun ambiguous-import-types (summary)
  (mapcar #'car (summary-ambiguous-imports summary)))

(defun ambiguous-import-for-type (type summary)
  (cadr (assoc type (summary-ambiguous-imports summary) :test #'string=)))

(defun qualify-dependecy (dependency)
  "Checks if DEPENDENCY can be expanded to it's fully qualified name.
If not it annotates the DEPENDECY by append a '*' to it."
  (let ((resolved (find-jclass dependency)))
    (typecase resolved
      (string
       ;; found it
       resolved)
      (t
       ;; either there are more than on Qname available (try find-jclass with String) or
       ;; it cannot be resolved. 
       (concatenate 'string dependency "*")))))

(defun add-type-dependency (summary type dependency)
  (aif (assoc type (summary-type-dependecies summary) :test #'string=)
       (pushnew (qualify-dependecy dependency) (cdr it) :test #'string=)
       (push (list type (qualify-dependecy dependency)) (summary-type-dependecies summary))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TYPE HIERARCHIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-superclasses (class)
  "Lists all superclasses of CLASS"
  (with-constant-signature ((get-name "getName"))
    (labels ((recu (current superclasses)
               (if current
                   (recu (jclass-superclass current)
                         (cons (get-name current) superclasses))
                   superclasses)))
      (nreverse (recu class nil)))))

(defun resolve-hierarchies (summary)
  "Tries to resolve the type hierarchies for SUMMARY-TYPES and SUMMARY-TYPE-DEPENDECIES."
  (let ((map nil))
    (loop for type in (summary-types summary)
       do (unless (assoc (first type) map :test #'string=)
            (setf map
                  (acons (first type)
                         (list-superclasses (ignore-errors (find-java-class (first type))))
                         map))))
    ;; somehow mapcan leaks when this function is called twice sequentially.
    ;; And I don't know why. Handling this otherwise.
    ;; (loop for type-dep in (mapcan #'identity (summary-type-dependecies summary))
    ;;    do (format t "Type dep: ~S~%" type-dep))
    (loop for type-deps in (summary-type-dependecies summary)
       do (loop for dep in type-deps
             do (unless (assoc dep map :test #'string=)
                  (setf map
                        (acons dep (list-superclasses (ignore-errors (find-java-class dep))) map)))))
    map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NAME RANKING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *similarity-threshold* 0.7
  "The similarity threshold.")

(defun rank-name-similarity (summary)
  (let ((types (summary-types summary))
        (test-name (summary-test-name summary)))
    (loop for (type nil) in types
       do
         (when (or (search type test-name)
                   (> (soerensen-dice-coefficient type test-name) *similarity-threshold*))
           (rank-type summary type)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INTERFACE FUNCTIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze (spec)
  (handler-case
      (let* ((compilation-unit (typecase spec
                                 ((or string pathname)
                                  (parse spec))
                                 (otherwise
                                  spec)))
             (summary (walk-ast compilation-unit)))
        (setf (summary-file-path summary) spec)
        (rank-name-similarity summary)
        (dolist (type (mapcar #'car (summary-types summary)))
          (resolve-import type summary)
          (collect-dependencies compilation-unit type summary))
        summary)
    (java-exception (condition)
      ;; TODO: test if condition is a ParserExsception
      (signal-parse-error spec (java-exception-cause condition)))))

(defun report (summary &key (stream *standard-output*) json xml)
  (cond
    ((and json xml)
     (restart-case (error "Cannot create reports in JSON and XML at the same time. Please decide!")
       (use-json ()
         :report "Report in JSON"
         (report summary :stream stream :json t))
       (use-xml ()
         :report "Report in XML"
         (report summary :stream stream :xml t))
       (use-prose ()
         :report "Report in prose"
         (report summary :stream stream))))
    (json
     (yason:encode summary stream))
    (xml
     (encode-xml summary stream))
    (t
     (format stream "Statistics for ~A~&File path: ~A~%~%"
             (summary-test-name summary) (summary-file-path summary))
     (format stream "Search subjects:~%")
     (loop for (type . rank) in (possible-search-subjects summary)
           do (format stream " ~A: ~A~&" rank type))
     (format stream "~%Type Dependencies:~%")
     (loop for (type . rest) in (summary-type-dependecies summary)
           do (format stream " ~A:~{ ~A~^,~}~&" type rest))
     (format stream "~%'*': Type is either ambiguous or cannot be resolved.~%")
     (format stream "~%Imports:~%~{ ~A~&~}" (summary-imports summary))
     (format stream "~%Ambiguous type imports:~%")
     (loop for (type imports) in (summary-ambiguous-imports summary)
           do (format stream "  ~A:~&~{    ~A~&~}" type imports))
     (format stream "~%Unresolved types:~%~{ ~A~&~}" (unresolved-types summary))
     (format stream "~%Identified type hierarchies:~%")
     (loop for hierarchy in (resolve-hierarchies summary)
           unless (= 1 (length hierarchy))
             do (format stream " ~A:~{ ~A ~^<~}~%" (first hierarchy) (rest hierarchy)))
     (format stream "~%Messages:~%~{  ~A~&~}" (summary-messages summary)))))
