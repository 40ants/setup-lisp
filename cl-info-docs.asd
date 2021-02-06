(defsystem example-docs
  :build-operation build-docs-op
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :depends-on ("example-docs/docs"))
