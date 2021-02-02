(defsystem cl-info-test
  :author ""
  :license ""
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("cl-info-test/core")
  :description "Test system for cl-info"

  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                           (error "Tests failed"))))
