(defpackage #:project-docs/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/docs)
  (:import-from #:40ants-ci/jobs/autotag)
  (:import-from #:40ants-ci/jobs/linter))
(in-package project-docs/ci)


(defworkflow release
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/autotag:autotag)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :by-cron "0 10 * * 1"
  :cache t
  :jobs ((40ants-ci/jobs/docs:build-docs)
         (40ants-ci/jobs/linter:linter :check-imports t)))
