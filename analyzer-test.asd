
(in-package :asdf-user)

(defsystem #:analyzer-test
  :author "Sebastian Christ <rudolfo.christ@gmail.com>"
  :license "GNU AGPL"
  :version "0.1.0"
  :depends-on (#:analyzer
               #:fiveam)
  :components ((:module "t"
                :components ((:file "basic")
                             (:file "string-similarity"))))
  :perform (test-op (op s)
                    (uiop:symbol-call :5am :run! :basic)))
