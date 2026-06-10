impty CLI: core operations on the base spec, against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ BASE=../../testdata/interp/impty/base

Typecheck a well-typed program:

  $ spectec impty typecheck --spec $SPEC -p $BASE/hello.imp --color never
  Typecheck succeeded

Evaluate it to a final environment:

  $ spectec impty eval --spec $SPEC -p $BASE/hello.imp --color never
  [
    y -> true,
    x -> 5
  ]

The SL interpreter (--sl) produces the same environment:

  $ spectec impty eval --spec $SPEC -p $BASE/hello.imp --color never --sl
  [
    y -> true,
    x -> 5
  ]

Parse it to an IL value:

  $ spectec impty parse --spec $SPEC -p $BASE/hello.imp --color never
  (((INT) (_ID "x") = (_NUM 5)) ; ((BOOL) (_ID "y") = ((_ID "x") <= (_NUM 10))))

A static type error renders a diagnostic to stderr and exits nonzero:

  $ spectec impty typecheck --spec $SPEC -p $BASE/_errors_undeclared.imp --color never
  error: invocation of relation Check_prog failed
    --> ../../specs/impty/base/spec.spectec:142:6
      |
  142 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../specs/impty/base/spec.spectec:142:6-142:19:
      |     invocation of relation Check_command failed
      |     └── ../../specs/impty/base/spec.spectec:142:6-142:19:
      |         application of rule Check_command/assign failed
      |         └── ../../specs/impty/base/spec.spectec:115:9-115:39:
      |             condition ($lookup<id, type>(tenv, x) = ?(t)) was not met
  [1]
