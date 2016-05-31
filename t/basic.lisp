(in-package :cl-user)

(defpackage #:analyzer.test
  (:use :cl :1am :analyzer :analyzer.parser :jss)
  (:import-from :alexandria
                #:switch))

(in-package :analyzer.test)

(defparameter *test-file*
  (merge-pathnames "example-tests/ArrayJoinTest.java"
                   (asdf:system-source-directory :analyzer)))

(declaim (inline string-assoc))
(defun string-assoc (item alist)
  (assoc item alist :test #'string=))

(test sanity-checks
  (let ((summary (analyze *test-file*)))
    ;; checking imports
    (is (member "org.junit.Test" (get-imports summary) :test #'string=))

    ;; checking global bindings
    (is (get-global-binding summary "array"))
    (is (get-global-binding summary "t"))
    (is (get-global-binding summary "t2"))

    ;; check local bindings for "test_simple_join"
    (dolist (bindings (css::summary-local-bindings summary))
      (destructuring-bind (scope &rest locals)
          bindings
        (switch (scope :test #'string=)
          ("test_simple_join"
           (is (string-assoc "j" locals)))
          ("test_join_with_comma"
           (is (string-assoc "j" locals)))
          ("not_interesting"
           (is (string-assoc "js" locals))
           (is (string-assoc "tt" locals)))
          ("identity_test"
           (is (string-assoc "a" locals))))))

    ;; check rankings
    (is (= 12 (cdr (string-assoc "Joiner" (possible-search-subjects summary)))))
    (is (= 6 (cdr (string-assoc "Toiner" (possible-search-subjects summary)))))

    ;; there are no more than two types
    (is (= 2 (length (possible-search-subjects summary))))))

(defparameter *test-template*
  "public class ~A {
@Test
public void test1() {
Joiner j = new Joiner();
Toiner t = new Toiner();
}}" "This template can be used as FORMAT string. You can provide a class name.")

(test test-name-ranking
  (let ((arbitrary-summary (analyze
                            (parse-string (format nil *test-template* "ArbitraryTest"))))
        (joiner-summary (analyze
                         (parse-string (format nil *test-template* "JoinerTest")))))
    (is (= 1 (cdr (string-assoc "Joiner" (possible-search-subjects arbitrary-summary)))))
    (is (= 1 (cdr (string-assoc "Toiner" (possible-search-subjects arbitrary-summary)))))

    (is (= 2 (cdr (string-assoc "Joiner" (possible-search-subjects joiner-summary)))))
    (is (= 1 (cdr (string-assoc "Toiner" (possible-search-subjects joiner-summary)))))))

(test direct-test-name-substring
  ;; ignore soerensen-dice-coefficient to test
  ;; direct substring ranking
  (let* ((*similarity-threshold* 1)
         (substring-summary (analyze
                             (parse-string (format nil *test-template* "MyJoinerTest"))))
         (no-substring-summary (analyze
                                (parse-string (format nil *test-template* "FoobarTest")))))
    (is (= 2 (cdr (string-assoc "Joiner" (possible-search-subjects substring-summary)))))
    (is (= 1 (cdr (string-assoc "Joiner" (possible-search-subjects no-substring-summary)))))))

(test static-calls-ranking
  (let* ((test-file (merge-pathnames "example-tests/LoginTest.java"
                                     (asdf:system-source-directory :analyzer)))
         (cu (parse test-file))
         (summary (analyze cu)))
    ;; Login ranked three times:
    ;; twice for the static call
    ;; once for name similarity
    (is (= 3 (cdr (string-assoc "Login" (possible-search-subjects summary)))))
    (is (= 2 (cdr (string-assoc "Session" (possible-search-subjects summary)))))))
