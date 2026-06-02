A loop body reading a never-declared variable fails the typechecker.

  $ ./main.exe ../../../specs/impty/base nested-undeclared-in-loop.imp
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
      |         application of rule Check_command/seq failed
      |         └── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |             invocation of relation Check_command failed
      |             └── ../../../specs/impty/base/spec.spectec:130:6-130:19:
      |                 application of rule Check_command/while failed
      |                 └── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |                     invocation of relation Check_command failed
      |                     └── ../../../specs/impty/base/spec.spectec:125:6-125:19:
      |                         application of rule Check_command/assign failed
      |                         └── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |                             invocation of relation Check_expr failed
      |                             └── ../../../specs/impty/base/spec.spectec:113:6-113:16:
      |                                 application of rule Check_expr/add failed
      |                                 └── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |                                     invocation of relation Check_expr failed
      |                                     └── ../../../specs/impty/base/spec.spectec:80:6-80:16:
      |                                         application of rule Check_expr/id failed
      |                                         └── ../../../specs/impty/base/spec.spectec:75:39-75:43:
      |                                             condition type'?{type' <- type'?} matches (_) was not met
  [1]
