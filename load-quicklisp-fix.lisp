(handler-bind ((serious-condition
                 (lambda (condition)
                   (uiop:print-condition-backtrace condition)
                   (uiop:quit 1))))
  (let ((fix-filename (merge-pathnames
                       (make-pathname :directory '(:relative ".quicklisp-client-fix")
                                      :name "quicklisp-fix"
                                      :type "lisp")
                       (user-homedir-pathname))))
    (let ((quicklisp-found #+quicklisp t
                           #-quicklisp nil))
      (cond
        ((not quicklisp-found)
         (warn "Quicklisp is not available, skipping fix loading.~%"))
        ((probe-file fix-filename)
         (handler-bind ((warning #'muffle-warning))
           (load fix-filename)))
        (t
         (warn "Quicklisp fix was not found at ~S.~%" fix-filename))))))
