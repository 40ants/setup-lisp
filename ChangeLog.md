<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C4-2E0-2E5-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 4.0.5 (2024-04-19)

<a id="fixed"></a>

### Fixed

* Extended test matrix with macos-14 runner.
* Fixed Roswell cache paths for macos-14 runner.
* Extended qlot cache key with runner architecture.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C4-2E0-2E4-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 4.0.4 (2024-04-18)

<a id="fixed"></a>

### Fixed

* Cache key was extended with runner architecture. Thanks for the contribution to Ákos Kiss!

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C4-2E0-2E3-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 4.0.3 (2024-03-12)

<a id="fixed"></a>

### Fixed

* There was an error when action suceeded, but actually failed trying to load my quicklisp fix for package inferred systems.
  A code to load this fix used `:HOME` as a pathname component which is not supported by some `CL` implementations. Actually,
  I've already fixed this issue in 3.1.0 version, but somehow this fix was lost. Also, a handler-bind wrapper around the loader was added
  and now action's tests will fail in case of any errors.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C4-2E0-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 4.0.2 (2024-02-27)

<a id="fixed"></a>

### Fixed

* Fix recreating of qlfile inside qlot environment cache.

When some jobs used qlfile-template argument + caching, a template application
  result was cached and caused an error during the next run.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C4-2E0-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 4.0.1 (2024-02-25)

<a id="fixed"></a>

### Fixed

* Fixed loading of quicklisp-osx-fix. Previously the script made unnecessary output and when you had a `qlfile` template in your workflow this output leaked into `qlfile` broking following `qlot install`.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C4-2E0-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 4.0.0 (2024-02-24)

<a id="changed"></a>

### Changed

* Internal cache mechanism was added. Now action caches Roswell and Qlot files to speed up reruns. For example, without cache action could be executed about 4 minutes, and with cache it runs only 20 seconds on Ubuntu or 1 minute on Windows.
* A new input variable `cache` was added to control caching beheviour. It is `true` by default, but you can switch it to `false` to turn caching off.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C3-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 3.2.0 (2024-01-30)

<a id="changed"></a>

### Changed

* Now action uses `bash -xeo pipefail` for running Roswell install script. This way, in case of some errors inside the script, it will be interrupted immediately.
* Also, we don't attempt to set `ROSWELL_INSTALL_DIR` env variable anymore, because despite it's presence, Roswell was installed into `/mingw64/bin/` instead under Windows and it works well (at least for me).
* Now we don't add action's directory to the `PATH` using modification of `GITHUB_PATH` variable. This prevents modification of the `PATH` of a workflow called the action.

<a id="fixed"></a>

### Fixed

* Fixed running of test.ros script under Windows.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C3-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 3.1.0 (2024-01-27)

<a id="changed"></a>

### Changed

* Switched from Roswell v21.10.14.111 to v23.10.14.114.
* Now action checks if it is really installed requested Lisp implementation.
* A list of supported implementation was added to the documentation.
* Some implementation supported by Roswell, have problems. If you need them, please, contribute the fix either to this action or to the Roswell itself.

<a id="fixed"></a>

### Fixed

* Now all steps fail-fast on the first encountered error. Previously some step might be failed, but action's execution was considered success.
* Fixed Quicklisp fix activation for lisp implementations which are not support `:HOME` as part of the pathname.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C3-2E0-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 3.0.0 (2023-12-14)

Now action uses a fix for Quicklisp client which makes it possible to load package inferred `ASDF` systems by name of the subsystem.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C2-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.1.0 (2022-11-10)

Moved to newer action msys2/setup-msys2@2.14.2 where a warning about stale Node.js version is fixed.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C2-2E0-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 2.0.0 (2021-10-28)

<a id="new"></a>

### New

* Add Windows support.

<a id="breaking-changes"></a>

### Breaking changes

Now instead of latest version of `ASDF`, Roswell and Qlot
we pin versions which known to work in supported environments.

If you want latest version, pass "latest" as value of
"asdf-version", "roswell-version" or "qlot-version".

Currently these versions are used:

* `ASDF`: 3.3.5.3
* Roswell: v21.10.14.111
* Qlot: 0.11.5

Thanks for these changes to Matteo Landi!

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C1-2E1-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.1.2 (2021-09-19)

Move from Qlot 0.11.1 to 0.11.5.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C1-2E1-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.1.1 (2021-09-12)

Version of qlot was fixed on 0.11.1, because later versions are broken and unable to install
systems from the GitHub. See issue [#118][2ffd].

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C1-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.1.0 (2021-06-06)

Added a new input variable `asdf-version`. By default, the latest
`ASDF` will be used.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C1-2E0-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.0.2 (2021-04-04)

Fixed installation of `libcurl4-openssl-dev` on Ubuntu Focal.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C1-2E0-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.0.1 (2021-02-22)

Fixed error when there is no `qlfile` in the repository.

<a id="x-28PROJECT-DOCS-2FCHANGELOG-3A-3A-7C1-2E0-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 1.0.0 (2021-01-07)

Initial version.


[2ffd]: https://github.com/fukamachi/qlot/issues/118

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
