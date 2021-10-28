(defpackage #:project-docs
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
  (ql:quickload :40ants-doc-theme-40ants)
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
      - uses: actions/checkout@v1
      - uses: 40ants/setup-lisp@v1
        with:
          asdf-system: cl-info
      - uses: 40ants/run-tests@v2-beta
        with:
          asdf-system: cl-info
```

The part, corresponding to an action call is:

```yaml
- uses: 40ants/setup-lisp@v1
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
- uses: 40ants/setup-lisp@v1
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
- uses: 40ants/setup-lisp@v1
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
- uses: 40ants/setup-lisp@v1
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
  - uses: actions/checkout@v1
  - uses: 40ants/setup-lisp@v1
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

To speed up build, you can use caching using a standad GitHub action `actions/cache@v2`.

To make caching work, add such sections into your workflow file:

```yaml
- name: Grant All Perms to Make Cache Restoring Possible
  run: |
    sudo mkdir -p /usr/local/etc/roswell
    sudo chown \"${USER}\" /usr/local/etc/roswell
    # Here the ros binary will be restored:
    sudo chown \"${USER}\" /usr/local/bin
- name: Get Current Month
  id: current-month
  run: |
    echo \"::set-output name=value::$(date -u \"+%Y-%m\")\"
- name: Cache Roswell Setup
  id: cache
  uses: actions/cache@v2
  env:
    cache-name: cache-roswell
  with:
    path: |
      /usr/local/bin/ros
      ~/.cache/common-lisp/
      ~/.roswell
      /usr/local/etc/roswell
      .qlot
    key: \"${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-${{ runner.os }}-${{ hashFiles('qlfile.lock') }}\"
- name: Restore Path To Cached Files
  run: |
    echo $HOME/.roswell/bin >> $GITHUB_PATH
    echo .qlot/bin >> $GITHUB_PATH
  if: steps.cache.outputs.cache-hit == 'true'
- uses: 40ants/setup-lisp@v1
  if: steps.cache.outputs.cache-hit != 'true'
```

There are two important lines here.

- The last line `if: steps.cache.outputs.cache-hit != 'true'` skips
  running lisp installation, it it was take from the cache.
- The `key` value:

  ```
  key: \"${{ steps.current-month.outputs.value }}-${{ env.cache-name }}-${{ runner.os }}-${{ hashFiles('qlfile.lock') }}\"
  ```

  It controls when your cache will be matched. If you are using `matrix`, put all it's components
  into the key.

  I also added a current month there, to make sure cache will be renewed at least monthly.
  This way a new Roswell, Qlot and ASDF will be used in a build.
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
