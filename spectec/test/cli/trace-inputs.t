impty CLI: trace inputs against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The inputs level adds the input values on enter and the output on exit:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --trace.level inputs
  [ 0] → Run_prog
  [ 0]     int x = 5 ; bool y = x <= 10
  [ 1]   → Run_prog/
  [ 1]   → Check_prog
  [ 1]       int x = 5 ; bool y = x <= 10
  [ 2]     → Check_prog/
  [ 2]     → Check_command
  [ 2]         []
  [ 2]         int x = 5 ; bool y = x <= 10
  [ 3]       → Check_command/skip
  [ 3]       ← Check_command/skip [fail]
  [ 3]       → Check_command/decl
  [ 3]       ← Check_command/decl [fail]
  [ 3]       → Check_command/assign
  [ 3]       ← Check_command/assign [fail]
  [ 3]       → Check_command/ite
  [ 3]       ← Check_command/ite [fail]
  [ 3]       → Check_command/while
  [ 3]       ← Check_command/while [fail]
  [ 3]       → Check_command/seq
  [ 3]       → Check_command
  [ 3]           []
  [ 3]           int x = 5
  [ 4]         → Check_command/skip
  [ 4]         ← Check_command/skip [fail]
  [ 4]         → Check_command/decl
  [ 4]         → Check_expr
  [ 4]             []
  [ 4]             5
  [ 5]           → Check_expr/num
  [ 5]           ← Check_expr/num [ok]
  [ 4]         ← Check_expr [ok]
  [ 4]             int
  [ 4]         ← Check_command/decl [ok]
  [ 3]       ← Check_command [ok]
  [ 3]           [ x -> int ]
  [ 3]       → Check_command
  [ 3]           [ x -> int ]
  [ 3]           bool y = x <= 10
  [ 4]         → Check_command/skip
  [ 4]         ← Check_command/skip [fail]
  [ 4]         → Check_command/decl
  [ 4]         → Check_expr
  [ 4]             [ x -> int ]
  [ 4]             x <= 10
  [ 5]           → Check_expr/num
  [ 5]           ← Check_expr/num [fail]
  [ 5]           → Check_expr/boollit
  [ 5]           ← Check_expr/boollit [fail]
  [ 5]           → Check_expr/id
  [ 5]           ← Check_expr/id [fail]
  [ 5]           → Check_expr/not
  [ 5]           ← Check_expr/not [fail]
  [ 5]           → Check_expr/add
  [ 5]           ← Check_expr/add [fail]
  [ 5]           → Check_expr/leq
  [ 5]           → Check_expr
  [ 5]               [ x -> int ]
  [ 5]               x
  [ 6]             → Check_expr/num
  [ 6]             ← Check_expr/num [fail]
  [ 6]             → Check_expr/boollit
  [ 6]             ← Check_expr/boollit [fail]
  [ 6]             → Check_expr/id
  [ 6]             → $lookup
  [ 6]                 [ x -> int ]
  [ 6]                 x
  [ 6]             ← $lookup
  [ 6]                 Some(int)
  [ 6]             ← Check_expr/id [ok]
  [ 5]           ← Check_expr [ok]
  [ 5]               int
  [ 5]           → Check_expr
  [ 5]               [ x -> int ]
  [ 5]               10
  [ 6]             → Check_expr/num
  [ 6]             ← Check_expr/num [ok]
  [ 5]           ← Check_expr [ok]
  [ 5]               int
  [ 5]           ← Check_expr/leq [ok]
  [ 4]         ← Check_expr [ok]
  [ 4]             bool
  [ 4]         ← Check_command/decl [ok]
  [ 3]       ← Check_command [ok]
  [ 3]           [ y -> bool, x -> int ]
  [ 3]       ← Check_command/seq [ok]
  [ 2]     ← Check_command [ok]
  [ 2]         [ y -> bool, x -> int ]
  [ 2]     ← Check_prog/ [ok]
  [ 1]   ← Check_prog [ok]
  [ 1]   → Eval_prog
  [ 1]       int x = 5 ; bool y = x <= 10
  [ 2]     → Eval_prog/
  [ 2]     → Eval_command
  [ 2]         []
  [ 2]         int x = 5 ; bool y = x <= 10
  [ 3]       → Eval_command/skip
  [ 3]       ← Eval_command/skip [fail]
  [ 3]       → Eval_command/decl
  [ 3]       ← Eval_command/decl [fail]
  [ 3]       → Eval_command/assign
  [ 3]       ← Eval_command/assign [fail]
  [ 3]       → Eval_command/ite-true
  [ 3]       ← Eval_command/ite-true [fail]
  [ 3]       → Eval_command/ite-false
  [ 3]       ← Eval_command/ite-false [fail]
  [ 3]       → Eval_command/while-false
  [ 3]       ← Eval_command/while-false [fail]
  [ 3]       → Eval_command/while-true
  [ 3]       ← Eval_command/while-true [fail]
  [ 3]       → Eval_command/seq
  [ 3]       → Eval_command
  [ 3]           []
  [ 3]           int x = 5
  [ 4]         → Eval_command/skip
  [ 4]         ← Eval_command/skip [fail]
  [ 4]         → Eval_command/decl
  [ 4]         → Eval_expr
  [ 4]             []
  [ 4]             5
  [ 5]           → Eval_expr/num
  [ 5]           ← Eval_expr/num [ok]
  [ 4]         ← Eval_expr [ok]
  [ 4]             5
  [ 4]         ← Eval_command/decl [ok]
  [ 3]       ← Eval_command [ok]
  [ 3]           [ x -> 5 ]
  [ 3]       → Eval_command
  [ 3]           [ x -> 5 ]
  [ 3]           bool y = x <= 10
  [ 4]         → Eval_command/skip
  [ 4]         ← Eval_command/skip [fail]
  [ 4]         → Eval_command/decl
  [ 4]         → Eval_expr
  [ 4]             [ x -> 5 ]
  [ 4]             x <= 10
  [ 5]           → Eval_expr/num
  [ 5]           ← Eval_expr/num [fail]
  [ 5]           → Eval_expr/boollit
  [ 5]           ← Eval_expr/boollit [fail]
  [ 5]           → Eval_expr/id
  [ 5]           ← Eval_expr/id [fail]
  [ 5]           → Eval_expr/not
  [ 5]           ← Eval_expr/not [fail]
  [ 5]           → Eval_expr/add
  [ 5]           ← Eval_expr/add [fail]
  [ 5]           → Eval_expr/leq
  [ 5]           → Eval_expr
  [ 5]               [ x -> 5 ]
  [ 5]               x
  [ 6]             → Eval_expr/num
  [ 6]             ← Eval_expr/num [fail]
  [ 6]             → Eval_expr/boollit
  [ 6]             ← Eval_expr/boollit [fail]
  [ 6]             → Eval_expr/id
  [ 6]             → $lookup
  [ 6]                 [ x -> 5 ]
  [ 6]                 x
  [ 6]             ← $lookup
  [ 6]                 Some(5)
  [ 6]             ← Eval_expr/id [ok]
  [ 5]           ← Eval_expr [ok]
  [ 5]               5
  [ 5]           → Eval_expr
  [ 5]               [ x -> 5 ]
  [ 5]               10
  [ 6]             → Eval_expr/num
  [ 6]             ← Eval_expr/num [ok]
  [ 5]           ← Eval_expr [ok]
  [ 5]               10
  [ 5]           ← Eval_expr/leq [ok]
  [ 4]         ← Eval_expr [ok]
  [ 4]             true
  [ 4]         ← Eval_command/decl [ok]
  [ 3]       ← Eval_command [ok]
  [ 3]           [ y -> true, x -> 5 ]
  [ 3]       ← Eval_command/seq [ok]
  [ 2]     ← Eval_command [ok]
  [ 2]         [ y -> true, x -> 5 ]
  [ 2]     ← Eval_prog/ [ok]
  [ 1]   ← Eval_prog [ok]
  [ 1]       [ y -> true, x -> 5 ]
  [ 1]   ← Run_prog/ [ok]
  [ 0] ← Run_prog [ok]
  [ 0]     [ y -> true, x -> 5 ]
  [
    y -> true,
    x -> 5
  ]
