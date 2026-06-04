An if-condition that is not a boolean fails the typechecker.

  $ ./main.exe ../../../specs/impty/base non-boolean-condition.imp
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
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/base/spec.spectec:131:6-131:19:
      |             invocation of relation Check_command failed
      |             └── ../../../specs/impty/base/spec.spectec:131:6-131:19:
      |                 application of rule Check_command/ite failed
      |                 └── ../../../specs/impty/base/spec.spectec:119:30-119:34:
      |                     condition type matches `BOOL` was not met
  [1]
