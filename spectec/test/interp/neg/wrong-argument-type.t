Calling a function with a wrong-typed argument fails the typechecker.

  $ ./main.exe ../../../specs/impty/closure wrong-argument-type.imp
  error: invocation of relation Check_prog failed
    --> ../../../specs/impty/closure/spec.spectec:156:6
      |
  156 |   -- Check_command: eps |- command -| tenv
      |      ^^^^^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Check_prog/ failed
      | └── ../../../specs/impty/closure/spec.spectec:156:6-156:19:
      |     invocation of relation Check_command failed
      |     └── ../../../specs/impty/closure/spec.spectec:156:6-156:19:
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/closure/spec.spectec:145:6-145:19:
      |             invocation of relation Check_command failed
      |             └── ../../../specs/impty/closure/spec.spectec:145:6-145:19:
      |                 application of rule Check_command/decl failed
      |                 └── ../../../specs/impty/closure/spec.spectec:124:6-124:16:
      |                     invocation of relation Check_expr failed
      |                     └── ../../../specs/impty/closure/spec.spectec:124:6-124:16:
      |                         application of rule Check_expr/call failed
      |                         └── ../../../specs/impty/closure/spec.spectec:109:32-109:37:
      |                             condition (type' = t_arg) was not met
  [1]
