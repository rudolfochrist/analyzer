;;; serialization.lisp

(in-package :analyzer)

(defmethod yason:encode ((p pathname) &optional (stream *standard-output*))
  (print (namestring p) stream))

(defmethod yason:encode ((s summary) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "name" (summary-test-name s))
      (yason:encode-object-element "path" (summary-file-path s))
      (yason:encode-object-element "imports" (summary-imports s))
      (yason:with-object-element ("subjects")
        (yason:with-object ()
          (loop for subject in (summary-types s)
             do (yason:encode-object-element (car subject)
                                             (cdr subject)))))
      (yason:with-object-element ("type_dependecies")
        (yason:with-object ()
          (loop for dep in (summary-type-dependecies s)
             do (yason:encode-object-element (car dep) (cdr dep)))))
      (yason:encode-object-element "ambiguous_imports" (summary-ambiguous-imports s))
      (yason:encode-object-element "unresolved_types" (unresolved-types s))
      (yason:encode-object-element "messages" (summary-messages s)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf who:*prologue* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))

(defgeneric encode-xml (object &optional stream)
  (:documentation "Serializes object into XML."))

(defmethod encode-xml ((s summary) &optional (stream *standard-output*))
  (who:with-html-output (stream nil :prologue t :indent t)
    (:summary :name (summary-test-name s) :file (summary-file-path s)
              (:imports
               (loop for import in (summary-imports s)
                  do (who:htm
                      (:import (who:str import)))))
              (:subjects
               (loop for subject in (summary-types s)
                  do (who:htm
                      (:subject :rank (cdr subject)
                                (who:str (car subject))))))
              (:type-dependecies
               (loop for dep in (summary-type-dependecies s)
                  do (who:htm
                      (:type-dependecy
                       (:name (who:str (car dep)))
                       (loop for type in (cdr dep)
                          do (who:htm
                              (:type (who:str type))))))))
              (:ambiguous-imports
               (loop for a-import in (summary-ambiguous-imports s)
                  do (who:htm
                      (:ambiguous-import (who:str a-import)))))
              (:unresolved-types
               (loop for type in (unresolved-types s)
                  do (who:htm
                      (:unresolved-type (who:str type)))))
              (:messages
               (loop for message in (summary-messages s)
                  do (who:htm
                      (:message (who:str message))))))))
