Calling a function with a wrong-typed argument fails the typechecker.

  $ ./main.exe ../../../specs/impty/closure wrong-argument-type.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/closure/spec.spectec:150:6
      |
  150 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/closure/spec.spectec:150:6-150:19:
      |     invocation of relation Check_command failed
      |     └── ../../../specs/impty/closure/spec.spectec:150:6-150:19:
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/closure/spec.spectec:140:6-140:19:
      |             invocation of relation Check_command failed
      |             └── ../../../specs/impty/closure/spec.spectec:140:6-140:19:
      |                 application of rule Check_command/decl failed
      |                 └── ../../../specs/impty/closure/spec.spectec:119:6-119:16:
      |                     invocation of relation Check_expr failed
      |                     └── ../../../specs/impty/closure/spec.spectec:119:6-119:16:
      |                         application of rule Check_expr/call failed
      |                         └── ../../../specs/impty/closure/spec.spectec:105:32-105:37:
      |                             condition (type' = t_arg) was not met
  [1]
