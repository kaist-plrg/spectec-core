Assigning to a variable that was never declared fails the typechecker.

  $ ./main.exe ../../../specs/impty/base undeclared-variable.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/base/spec.spectec:141:6
      |
  141 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |     invocation of relation Check_command failed
      |     └── ../../../specs/impty/base/spec.spectec:141:6-141:19:
      |         application of rule Check_command/assign failed
      |         └── ../../../specs/impty/base/spec.spectec:114:9-114:43:
      |             condition ($lookup_<id, type>(tenv, x) = ?(type)) was not met
  [1]
