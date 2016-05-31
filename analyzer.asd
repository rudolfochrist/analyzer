
#-abcl (error "This code only works with ABCL!")
(require 'abcl-contrib)
(require 'abcl-asdf)

(in-package :asdf-user)

(defsystem #:analyzer
  :version      "0.1.0"
  :description  "Gather and summarize various information about test specifications."
  :author       "Sebastian Christ <rudolfo.christ@gmail.com>"
  :serial       t
  :license      "GNU AGPL"
  :components   ((:module "lisp"
                  :serial t
                  :components
                          ((:file "packages")
                           (:file "conditions")
                           (:file "parser")
                           (:file "soerensen-dice")
                           (:file "util")
                           (:file "types")
                           (:file "visitor")
                           (:file "analyzer-visitor")
                           (:file "method-call-visitor")
                           (:file "dependency-visitor")
                           (:file "analyzer")
                           (:file "serialization"))))
  :depends-on   (#:alexandria
                 #:javaparser-core
                 #:analyzer-java
                 #:jss
                 #:anaphora
                 #:yason
                 #:cl-who
                 #:unix-opts
                 #:cl-ppcre)
  :in-order-to ((test-op (test-op analyzer-test))))
