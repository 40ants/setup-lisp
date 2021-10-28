(defpackage #:project-docs/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/docs))
(in-package project-docs/ci)


(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :jobs ((40ants-ci/jobs/docs:build-docs)))
