(uiop:define-package #:project-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package project-docs/changelog)


(defchangelog (:ignore-words ("ASDF"
                              "PATH"
                              "HOME"))
  (4.0.0 2024-02-24
         "
# Changed

* Internal cache mechanism was added. Now action caches Roswell and Qlot files to speed up reruns. For example, without cache action could be executed about 4 minutes, and with cache it runs only 20 seconds on Ubuntu or 1 minute on Windows.
* A new input variable `cache` was added to control caching beheviour. It is `true` by default, but you can switch it to `false` to turn caching off.

")
  (3.2.0 2023-01-30
         "
# Changed

* Now action uses `bash -xeo pipefail` for running Roswell install script. This way, in case of some errors inside the script, it will be interrupted immediately.
* Also, we don't attempt to set `ROSWELL_INSTALL_DIR` env variable anymore, because despite it's presence, Roswell was installed into `/mingw64/bin/` instead under Windows and it works well (at least for me).
* Now we don't add action's directory to the `PATH` using modification of `GITHUB_PATH` variable. This prevents modification of the `PATH` of a workflow called the action.

# Fixed

* Fixed running of test.ros script under Windows.

")
  (3.1.0 2023-01-27
         "
# Changed

* Switched from Roswell v21.10.14.111 to v23.10.14.114.
* Now action checks if it is really installed requested Lisp implementation.
* A list of supported implementation was added to the documentation.
* Some implementation supported by Roswell, have problems. If you need them, please, contribute the fix either to this action or to the Roswell itself.

# Fixed

* Now all steps fail-fast on the first encountered error. Previously some step might be failed, but action's execution was considered success.
* Fixed Quicklisp fix activation for lisp implementations which are not support :HOME as part of the pathname.
")
  (3.0.0 2023-12-14
         "Now action uses a fix for Quicklisp client which makes it possible to load package inferred ASDF systems by name of the subsystem.")
  (2.1.0 2022-11-10
         "Moved to newer action msys2/setup-msys2@2.14.2 where a warning about stale Node.js version is fixed.")
  (2.0.0 2021-10-28
         "# New

          - Add Windows support.

          # Breaking changes

          Now instead of latest version of ASDF, Roswell and Qlot
          we pin versions which known to work in supported environments.

          If you want latest version, pass \"latest\" as value of
          \"asdf-version\", \"roswell-version\" or \"qlot-version\".

          Currently these versions are used:

          - ASDF: 3.3.5.3
          - Roswell: v21.10.14.111
          - Qlot: 0.11.5

          Thanks for these changes to Matteo Landi!")
  (1.1.2 2021-09-19
         "Move from Qlot 0.11.1 to 0.11.5.")
  (1.1.1 2021-09-12
         "Version of qlot was fixed on 0.11.1, because later versions are broken and unable to install
          systems from the GitHub. See issue [#118](https://github.com/fukamachi/qlot/issues/118).")
  (1.1.0 2021-06-06
         "Added a new input variable `asdf-version`. By default, the latest
          ASDF will be used.")
  (1.0.2 2021-04-04
         "Fixed installation of `libcurl4-openssl-dev` on Ubuntu Focal.")
  (1.0.1 2021-02-22
         "Fixed error when there is no `qlfile` in the repository.")
  (1.0.0 2021-01-07
         "Initial version."))
