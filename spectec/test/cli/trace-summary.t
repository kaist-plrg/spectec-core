impty CLI: trace summary against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Summary trace prints relation and function enter/exit only:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --trace.level summary
  [ 0] → Run_prog
  [ 1]   → Check_prog
  [ 2]     → Check_command
  [ 3]       → Check_command
  [ 4]         → Check_expr
  [ 4]         ← Check_expr [ok]
  [ 3]       ← Check_command [ok]
  [ 3]       → Check_command
  [ 4]         → Check_expr
  [ 5]           → Check_expr
  [ 6]             → $lookup
  [ 6]             ← $lookup
  [ 5]           ← Check_expr [ok]
  [ 5]           → Check_expr
  [ 5]           ← Check_expr [ok]
  [ 4]         ← Check_expr [ok]
  [ 3]       ← Check_command [ok]
  [ 2]     ← Check_command [ok]
  [ 1]   ← Check_prog [ok]
  [ 1]   → Eval_prog
  [ 2]     → Eval_command
  [ 3]       → Eval_command
  [ 4]         → Eval_expr
  [ 4]         ← Eval_expr [ok]
  [ 3]       ← Eval_command [ok]
  [ 3]       → Eval_command
  [ 4]         → Eval_expr
  [ 5]           → Eval_expr
  [ 6]             → $lookup
  [ 6]             ← $lookup
  [ 5]           ← Eval_expr [ok]
  [ 5]           → Eval_expr
  [ 5]           ← Eval_expr [ok]
  [ 4]         ← Eval_expr [ok]
  [ 3]       ← Eval_command [ok]
  [ 2]     ← Eval_command [ok]
  [ 1]   ← Eval_prog [ok]
  [ 0] ← Run_prog [ok]
  [
    y -> true,
    x -> 5
  ]
