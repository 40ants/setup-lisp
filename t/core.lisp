(defpackage #:cl-info-test/core
  (:use #:cl)
  (:import-from #:cl-info)
  (:import-from #:hamcrest/rove
                #:contains
                #:assert-that)
  (:import-from #:rove
                #:testing
                #:deftest))
(in-package cl-info-test/core)


(defun foo (a b)
  #+sbcl
  (sb-ext:describe-compiler-policy )
  
  (list a b 3))


(deftest test-some-staff
  (testing "Replace this test with real staff."
    (assert-that (foo 1 2)
                 (contains 1 2))))
