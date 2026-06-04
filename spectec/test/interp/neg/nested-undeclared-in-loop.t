A loop body reading a never-declared variable fails the typechecker.

  $ ./main.exe ../../../specs/impty/base nested-undeclared-in-loop.imp
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
      |                 application of rule Check_command/while failed
      |                 └── ../../../specs/impty/base/spec.spectec:126:6-126:19:
      |                     invocation of relation Check_command failed
      |                     └── ../../../specs/impty/base/spec.spectec:126:6-126:19:
      |                         application of rule Check_command/assign failed
      |                         └── ../../../specs/impty/base/spec.spectec:114:6-114:16:
      |                             invocation of relation Check_expr failed
      |                             └── ../../../specs/impty/base/spec.spectec:114:6-114:16:
      |                                 application of rule Check_expr/add failed
      |                                 └── ../../../specs/impty/base/spec.spectec:85:6-85:16:
      |                                     invocation of relation Check_expr failed
      |                                     └── ../../../specs/impty/base/spec.spectec:85:6-85:16:
      |                                         application of rule Check_expr/id failed
      |                                         └── ../../../specs/impty/base/spec.spectec:76:38-76:39:
      |                                             condition type?{type <- type?} matches (_) was not met
  [1]
