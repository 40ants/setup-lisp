(defpackage #:cl-info-test/core
  (:use #:cl
        #:cl-info/core
        #:rove
        #:hamcrest/rove))
(in-package cl-info-test/core)


(deftest test-some-staff
    (testing "Replace this test with real staff."
      (assert-that (foo 1 2)
                   (contains 1 2))))
