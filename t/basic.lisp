(in-package :cl-user)

(defpackage #:analyzer.test
  (:use :cl :5am :analyzer :analyzer.parser :jss)
  (:import-from :alexandria
                #:switch))

(in-package :analyzer.test)

(def-suite basic
  :description "Some basic tests about the analyzer.")

(in-suite basic)


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
  ;; They rank as normal object creations. This used to be different.
  (let* ((test-file (merge-pathnames "example-tests/LoginTest.java"
                                     (asdf:system-source-directory :analyzer)))
         (cu (parse test-file))
         (summary (analyze cu)))
    ;; Login ranked twice because of name similarity.
    (is (= 2 (cdr (string-assoc "Login" (possible-search-subjects summary)))))
    (is (= 2 (cdr (string-assoc "Session" (possible-search-subjects summary)))))))


(test resolve-correct-scopes
  ;; resolves to the right scope for method invocations.
  (let ((sum (analyze (merge-pathnames "example-tests/ThisTest.java"
                                       (asdf:system-source-directory :analyzer)))))
    (loop for type in '("BoardA" "BoardB" "BoardC" "BoardD")
          do
             ;; each type is ranked twice.
             ;; 1. The field initialization with new
             ;; 2. The usage int he method calls
             (is (= 2 (cdr (string-assoc type (possible-search-subjects sum))))))
    ;; BoardE is only used in a static call in should only ranked once.
    (is (= 1 (cdr (string-assoc "BoardE" (possible-search-subjects sum)))))))

(test ranking-on-method-calls
  (let* ((code "
public class Foo {
    private Bar bar = new Bar();

    @Test
    public void test_it() {
        bar.doStuff();
        this.bar.doStuff();
    }
}")
         (sum (analyze (parse-string code))))
    ;; the creation an both method calls should be ranked
    (is (= 3 (cdr (string-assoc "Bar" (possible-search-subjects sum)))))))

(test ranking-method-calls-in-assertions
  (let* ((code "
public class FooTest {
    Bar bar = new Bar();

    @Test
    public void test_it() {
        assertTrue(bar.hasQuux());
        assertTrue(this.bar.hasQuux());
    }
}")
         (sum (analyze (parse-string code))))
    (is (= 3 (cdr (string-assoc "Bar" (possible-search-subjects sum)))))))

(test obj-creation-rank-supertypes
  (let* ((code "
public class TestTestTest {
    private Oglo o = new Oglo();
    private Mago m = new Nago();

    @Test
    public void test() {
        Foo f = new Foo();
        Baz b = new Bar();

        Loo l;
        Noo n;
        l = new Loo();
        n = new Moo();
    }
}")
         (sum (analyze (parse-string code))))
    (loop for class in '("Foo" "Baz" "Bar" "Loo" "Noo" "Moo" "Oglo" "Mago" "Nago")
          do (is (= 1 (cdr (string-assoc class (possible-search-subjects sum))))))))

(test ignored-types-ranking
  (let* ((sum (analyze (merge-pathnames #p"example-tests/IgnoreTypesTest.java"
                                        (asdf:system-source-directory :analyzer))))
         (subjects (possible-search-subjects sum)))
    (is (= 2 (cdr (string-assoc "Foo" subjects))))

    ;; those aren't ranked
    (is (null (string-assoc "String" subjects)))
    (is (null (string-assoc "List" subjects)))
    (is (null (string-assoc "ArrayList" subjects)))
    (is (null (string-assoc "Assert" subjects)))

    ;; check also if generics have been included accidentally
    (is (null (string-assoc "List<String>" subjects)))
    (is (null (string-assoc "ArrayList<String>" subjects)))))

(test java-package-predicates
  (is (every #'identity
             (list (java-lang-p "String")
                   (java-lang-p "java.lang.Stringieuwe")
                   (java-util-p "Map")
                   (java-util-p "java.util.Set")
                   (java-util-p "Map<String, String>")
                   (java-util-p "java.util.Set<Number>")
                   (junit-p "Assert")
                   (junit-p "org.junit.Test")
                   (junit-p "org.hamcrest.CoreMatchers")
                   (junit-p "CoreMatchers")))))

(test error-on-security-breach
  (signals security-breach-error
    (analyzer::rank-type (analyzer::make-summary) "ServerSocket")))
