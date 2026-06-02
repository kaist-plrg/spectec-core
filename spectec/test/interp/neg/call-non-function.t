Calling a non-function value fails the typechecker.

  $ ./main.exe ../../../specs/impty/closure call-non-function.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/closure/spec.spectec:149:6
      |
  149 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |     invocation of relation Check_command failed
      |     └── ../../../specs/impty/closure/spec.spectec:149:6-149:19:
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |             invocation of relation Check_command failed
      |             └── ../../../specs/impty/closure/spec.spectec:139:6-139:19:
      |                 application of rule Check_command/decl failed
      |                 └── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |                     invocation of relation Check_expr failed
      |                     └── ../../../specs/impty/closure/spec.spectec:118:6-118:16:
      |                         application of rule Check_expr/call failed
      |                         └── ../../../specs/impty/closure/spec.spectec:103:32-103:52:
      |                             condition type matches `% -> %` was not met
  [1]
