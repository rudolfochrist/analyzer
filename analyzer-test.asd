
(in-package :asdf-user)

(defsystem #:analyzer-test
  :author "Sebastian Christ <rudolfo.christ@gmail.com>"
  :license "GNU AGPL"
  :depends-on (#:analyzer
               #:1am)
  :components ((:module "t"
                :components ((:file "basic")
                             (:file "string-similarity"))))
  :perform (test-op (op s)
                    (uiop:symbol-call :1am :run)))
