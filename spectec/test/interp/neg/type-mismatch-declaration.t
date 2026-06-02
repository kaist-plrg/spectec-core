A declaration whose initializer type differs from the declared type fails.

  $ ./main.exe ../../../specs/impty/base type-mismatch-declaration.imp
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
      |         └── ../../../specs/impty/base/spec.spectec:109:30-109:34:
      |             condition (type' = type) was not met
  [1]
