(handler-bind ((serious-condition
                 (lambda (condition)
                   (uiop:print-condition-backtrace condition)
                   (uiop:quit 1))))
  (let ((debug (equal (uiop:getenv "RUNNER_DEBUG")
                      "1"))
        (fix-filename (merge-pathnames
                       (make-pathname :directory '(:relative ".quicklisp-client-fix")
                                      :name "quicklisp-fix"
                                      :type "lisp")
                       (user-homedir-pathname))))
    (let ((quicklisp-found #+quicklisp t
                           #-quicklisp nil))
      (when debug
        (format *error-output*
                "Quicklisp found: ~A~%"
                quicklisp-found))
      (cond
        ((not quicklisp-found)
         (when debug
           (format *error-output*
                   "Quicklisp is not available."))
         (warn "Quicklisp is not available, skipping fix loading.~%"))
        ((probe-file fix-filename)
         (handler-bind ((warning #'muffle-warning))
           (when debug
             (format *error-output*
                     "Loading file ~A.~%"
                     fix-filename))
           (load fix-filename)))
        (t
         (when debug
           (format *error-output*
                   "File ~A not found.~%"
                   fix-filename))
         (warn "Quicklisp fix was not found at ~S.~%" fix-filename))))))
