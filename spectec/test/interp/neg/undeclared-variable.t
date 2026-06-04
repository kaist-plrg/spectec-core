Assigning to a variable that was never declared fails the typechecker.

  $ ./main.exe ../../../specs/impty/base undeclared-variable.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/base/spec.spectec:142:6
      |
  142 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/base/spec.spectec:142:6-142:19:
      |     invocation of relation Check_command failed
      |     └── ../../../specs/impty/base/spec.spectec:142:6-142:19:
      |         application of rule Check_command/assign failed
      |         └── ../../../specs/impty/base/spec.spectec:115:9-115:39:
      |             condition ($lookup<id, type>(tenv, x) = ?(t)) was not met
  [1]
