#|
  This file is a part of cl-info project.
|#

(defsystem cl-info-test
           :author ""
           :license ""
           :class :package-inferred-system
           :pathname "t"
           :depends-on (:cl-info
                        "cl-info-test/core")
           :description "Test system for cl-info"

           :perform (test-op :after (op c)
                             (symbol-call :rove :run c)
                             (clear-system c)))
