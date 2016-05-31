
(in-package :asdf-user)

(defsystem #:analyzer-java
  :version      "0.1.0"
  :description  "Code for the analyzer system that has to be in Java."
  :author       "Sebastian Christ <rudolfo.christ@gmail.com>"
  :serial       t
  :license      "GNU AGPL"
  :components   ((:mvn "com.fyi/analyzer" :version "0.1.0"))
  :defsystem-depends-on (:abcl-asdf))
