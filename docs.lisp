(uiop:define-package #:project-docs
  (:use #:cl)
  (:nicknames #:project-docs/docs)
  
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:project-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package project-docs)


(defmethod docs-config ((system (eql (asdf:find-system "project-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (uiop:symbol-call :ql :quickload
                    :40ants-doc-theme-40ants)
  #-quicklisp
  (asdf:load-system :40ants-doc-theme-40ants)
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "GitHub Action to Setup Common Lisp for CI"
                    :ignore-words ("OSX"
                                   "LISP"
                                   "ASDF"
                                   "CI"
                                   "CLPM"
                                   "PATH"))
  "
This is a Github Action to setup Common Lisp, Roswell and Qlot.

It is useful to call it before running tests or building docs
for your Common Lisp libraries. Action encapsulates all steps
necessary to make available [Roswell](https://github.com/roswell/roswell)
and [Qlot](https://github.com/fukamachi/qlot) inside the Github CI.
"
  (@features section)
  (@implementation-support section)
  (@typical-usage section)
  (@roswell-version section)
  (@asdf-version section)
  (@qlot-version section)
  (@ql-file section)
  (@caching section)
  (@roadmap section)
  (@contribution section))


(defsection-copy @readme @index)


(defsection @features (:title "What this action does for you?")
  "
* It installs Roswell and all it's dependencies, doing right thing depending on
  the operating system. It should work on Ubuntu, OSX and Windows.
* Upgrade ASDF to the latest version.
* Installs Qlot.
* Adds to `PATH` these directories: `~/.roswell/bin` and `.qlot/bin`
* Creates `.qlot` by running `qlot install`. How to override content of the
  `qlfile`, see \"Overriding qlfile\" section.
* And finally, it can install a specified ASDF system and all it's dependencies.
  But this step is optional.
* Installed Roswell, `.qlot` and `~/.cache/common-lisp/` files are cached to speed up
  repeated builds.
")


(defsection @implementation-support (:title "Implementation support")
  "
Most implementations are tested on Linux, but for some of them Windows and OSX are also should work.

Note, that for correct execution, your workflow should use `lispsh -eo pipefail` instead of default `bash`.
This way a workflow will work Linux, OSX and Windows. You you will ignore this advice, you'll see such error
when trying to call `ros` or `qlot` scripts:

```
/c/Users/runneradmin/.roswell/lisp/quicklisp/bin/qlot: line 4: exec: ros: not found
  Error: Process completed with exit code 127.
```

| **Implementation** | **Linux**                                            | **OSX** | **Windows**  |
|--------------------|------------------------------------------------------|---------|--------------|
| abcl-bin           | ✅                                                   | ✅      | [❌](https://github.com/40ants/setup-lisp/issues/27) |
| allegro            | [❌](https://github.com/40ants/setup-lisp/issues/22) |         |              |
| ccl-bin            | ✅                                                   | ✅      | [❌](https://github.com/40ants/setup-lisp/issues/27) |
| clasp              | [❌](https://github.com/40ants/setup-lisp/issues/16) |         | [❌](https://github.com/40ants/setup-lisp/issues/27) |
| clasp-bin          | ✅                                                   | [❌](https://github.com/40ants/setup-lisp/issues/29) |
| clisp              | [❌](https://github.com/40ants/setup-lisp/issues/15) |         | [❌](https://github.com/40ants/setup-lisp/issues/27) |
| clisp-head         | ✅                                                   | [❌](https://github.com/40ants/setup-lisp/issues/28) |
| cmu-bin            | ✅                                                   | [❌](https://github.com/40ants/setup-lisp/issues/30) | [❌](https://github.com/40ants/setup-lisp/issues/27) |
| ecl                | ✅                                                   | ✅      | [❌](https://github.com/40ants/setup-lisp/issues/27) |
| mkcl               | [❌](https://github.com/40ants/setup-lisp/issues/17) |         |              |
| npt                | [❌](https://github.com/40ants/setup-lisp/issues/18) |         |              |
| sbcl               | ✅                                                   |         |              |
| sbcl-bin           | ✅                                                   | ✅      | ✅           |

")

(defsection @typical-usage (:title "A typical usage")
  "
Here is how a minimal GitHub Workflow might look like:

```yaml
name: 'CI'

on:
  push:
    branches:
      - 'main'
      - 'master'
  pull_request:

jobs:
  tests:
    runs-on: ubuntu-latest
    
    strategy:
      matrix:
        lisp:
          - sbcl-bin
          - ccl-bin
          
    env:
      LISP: ${{ matrix.lisp }}

    steps:
      - uses: actions/checkout@v4
      - uses: 40ants/setup-lisp@v4
        with:
          asdf-system: cl-info
      - uses: 40ants/run-tests@v2
        with:
          asdf-system: cl-info
```

The part, corresponding to an action call is:

```yaml
- uses: 40ants/setup-lisp@v4
  with:
    asdf-system: cl-info
```

If you remove `with` part, then action will skip the ASDF system
installation.

Also, pay attention to the `env` section of the workflow. If you don't
set up a `LISP` env variable, action will set default lisp implementation
to `sbcl`:

```yaml
env:
  LISP: ${{ matrix.lisp }}
```

The last step in this workflow runs tests for the specified ASDF
system. It is documented [here](https://40ants.com/run-tests).
")


(defsection @roswell-version (:title "Overriding Roswell version")
  "
By default this action will install the latest version of Roswell known to be
working with this action. However, should you need to use a different version
instead, you can specify that via the `roswell-version` argument:

```
- uses: 40ants/setup-lisp@v4
  with:
    roswell-version: v21.10.14.111
```
")


(defsection @asdf-version (:title "Overriding ASDF version")
  "
By default this action will install the latest version of ASDF known to be
working with this action.  However, should you need to use a different version
instead, you can specify that via the `asdf-version` argument:

```
- uses: 40ants/setup-lisp@v4
  with:
    asdf-version: 3.3.5.3
```
")


(defsection @qlot-version (:title "Overriding Qlot version")
  "
By default this action will install the latest version of Qlot known to be
working with this action.  However, should you need to use a different version
instead, you can specify that via the `qlot-version` argument:

```
- uses: 40ants/setup-lisp@v4
  with:
    qlot-version: 0.11.5
```
")


(defsection @ql-file (:title "Overriding qlfile")
  "
Sometimes you might want to generate content of qlfile
depending on matrix parameters. For example with matrix like this one:

```yaml
matrix:
  os:
    - ubuntu-latest
    - macos-latest
    - windows-latest
  quicklisp-dist:
    - quicklisp
    - ultralisp
  lisp:
    - sbcl-bin
    - ccl-bin
    - ecl
```

you might want to add an [ultralisp](https://ultralisp.org) source
to the qlfile. Here is how this can be archived:

```yaml
env:
  LISP: ${{ matrix.lisp }}
  OS: ${{ matrix.os }}
  QUICKLISP_DIST: ${{ matrix.quicklisp-dist }}

steps:
  - uses: actions/checkout@v4
  - uses: 40ants/setup-lisp@v4
    with:
      asdf-system: cl-info
      qlfile-template: |
        {% ifequal quicklisp_dist \"ultralisp\" %}
        dist ultralisp http://dist.ultralisp.org
        {% endifequal %}
```

Here we see a few important things.

1. We put into the env var the type of the quicklisp distribution we want to
   our library to test against.
2. We pass a multiline argument `qlfile-template` to the action.
3. Template refers to `quicklisp_dist` to conditionally include a line
   into `qlfile` when `quicklisp_dist == \"ultralisp\"`.

You can refer any environment variable inside the `qlfile` templater.
Also note, it is using [Djula](https://github.com/mmontone/djula)
markup, similar to [Django](https://docs.djangoproject.com/en/3.1/topics/templates/)
and [Jinja2](https://jinja.palletsprojects.com/).
")


(defsection @caching (:title "Caching")
  "
Usually installing Roswell, a lisp implementation and dependencies
take from 2 to 10 minutes. Multiply this to the number of
matrix combinations and you'll get signifficant time.

Starting from version `4.0.0`, this action cares about caching itself
and you don't need to wrap it with `actions/cache`. This behaviour
of enabled by default. Without cache action could be executed about
4 minutes, and with cache it runs only 20 seconds on Ubuntu or 1 minute on Windows.

A new input variable `cache` was added to control caching beheviour.
It is `true` by default, but you can switch it to `false` to turn caching off.

The current month is used as part of the cache key, to refresh caches every month.
This way a new Roswell, Qlot and ASDF will be used in a build. Also, you can set
`env.cache-name` variable to some value, to force rebuild with a fresh cache.
")


(defsection @roadmap (:title "Roadmap")
  "
- Support [CLPM](https://gitlab.common-lisp.net/clpm/clpm).
- Vendor all dependencies, to make action more reliable and secure.
")

(defsection @contribution (:title "Contribution")
  "
If you want to contribute to this system, join development at GitHub:

<https://github.com/40ants/setup-lisp>
")
