
(in-package :asdf-user)

(defsystem #:javaparser-core
  :version      "2.3.0"
  :defsystem-depends-on (:abcl-asdf)
  :components   ((:mvn "com.github.javaparser/javaparser-core"
                       :version "2.3.0")))
