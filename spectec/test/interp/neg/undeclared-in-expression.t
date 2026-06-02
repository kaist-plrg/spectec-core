An initializer that reads a never-declared variable fails the typechecker.

  $ ./main.exe ../../../specs/impty/base undeclared-in-expression.imp
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
      |         application of rule Check_command/decl failed
      |         └── ../../../specs/impty/base/spec.spectec:109:6-109:16:
      |             invocation of relation Check_expr failed
      |             └── ../../../specs/impty/base/spec.spectec:109:6-109:16:
      |                 application of rule Check_expr/id failed
      |                 └── ../../../specs/impty/base/spec.spectec:75:39-75:43:
      |                     condition type'?{type' <- type'?} matches (_) was not met
  [1]
