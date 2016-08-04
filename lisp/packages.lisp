
(in-package :cl-user)

(defpackage #:analyzer.parser
  (:nicknames #:css.p)
  (:use :cl
        :jss
        :anaphora)
  (:export #:parse
           #:parser-print
           #:parse-raw
           #:parse-string))

(defpackage #:analyzer
  (:nicknames #:css)
  (:use :cl
   :analyzer.parser
        :anaphora
   :java
        :jss)
  (:import-from :alexandria
                #:when-let)
  ;; Summary accessor
  (:export
   #:get-binding
   #:get-global-binding
   #:get-local-binding
   #:get-imports)
  (:export
   #:soerensen-dice-coefficient
   #:analyze
   #:*similarity-threshold*
   #:possible-search-subjects
   #:print-messages
   #:unresolved-types
   #:ambiguous-import-types
   #:ambiguous-import-for-type
   #:report
   #:version
   #:parse-error
   #:parse-error-file
   #:parse-error-line
   #:parse-error-column
   #:parse-error-cause
   #:java-lang-p
   #:java-util-p
   #:junit-p
   #:security-breach-error))
