(defpackage #:docs
  (:use #:cl)
  (:nicknames #:docs/docs)
  (:import-from #:mgl-pax-minimal
                #:defsection
                #:section))
(in-package docs)


(defsection @index (:title "GitHub Action to Setup Common Lisp for CI")
  "
This is a Github Action to setup Common Lisp, Roswell and Qlot.

It is useful to call it before running tests or building docs
for your Common Lisp libraries. Action encapsulates all steps
necessary to make available [Roswell](https://github.com/roswell/roswell)
and [Qlot](https://github.com/fukamachi/qlot) inside the Github CI.
"
  (@features section)
  (@typical-usage section)
  (@ql-file section)
  (@roadmap section))


(defsection @features (:title "What this action does for you?")
  "
* It installs Roswell and all it's dependencies, doing right thing depending on
  the operating system. It should work on Ubuntu, OSX and maybe Windows.
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


(defsection @ql-file (:title "Overriding qlfile")
  "
Sometimes you might want to generate content of qlfile
depending on matrix parameters. For example with matrix like this one:

```yaml
matrix:
  os:
    - ubuntu-latest
    - macos-latest
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

        github mgl-pax svetlyak40wt/mgl-pax :branch mgl-pax-minimal
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


(defsection @roadmap (:title "Roadmap")
  "
- Make action use caching to speedup dependencies installation.
- Support [CLPM](https://gitlab.common-lisp.net/clpm/clpm).
- Vendor all dependencies, to make action more reliable and secure.
")
