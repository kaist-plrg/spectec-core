impty CLI: trace full against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The full level adds premises and clauses (the inner-loop detail). Iteration
markers are unreachable here: the base spec has no executed iterated premise.

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --trace.level full
  [ 0] → Run_prog
  [ 0]     int x = 5 ; bool y = x <= 10
  [ 1]   → Run_prog/
  [ 1]     | -- if Check_prog: |- command holds
  [ 1]   → Check_prog
  [ 1]       int x = 5 ; bool y = x <= 10
  [ 2]     → Check_prog/
  [ 2]       | -- rel Check_command: [] |- command -| tenv
  [ 2]     → Check_command
  [ 2]         []
  [ 2]         int x = 5 ; bool y = x <= 10
  [ 3]       → Check_command/skip
  [ 3]         | -- if command matches `SKIP`
  [ 3]       ← Check_command/skip [fail]
  [ 3]       → Check_command/decl
  [ 3]         | -- if command matches `% % = %`
  [ 3]       ← Check_command/decl [fail]
  [ 3]       → Check_command/assign
  [ 3]         | -- if command matches `% = %`
  [ 3]       ← Check_command/assign [fail]
  [ 3]       → Check_command/ite
  [ 3]         | -- if command matches `IF % THEN % ELSE % END`
  [ 3]       ← Check_command/ite [fail]
  [ 3]       → Check_command/while
  [ 3]         | -- if command matches `WHILE % DO % END`
  [ 3]       ← Check_command/while [fail]
  [ 3]       → Check_command/seq
  [ 3]         | -- if command matches `% ; %`
  [ 3]         | -- let c_1 ; c_2 = command
  [ 3]         | -- rel Check_command: tenv |- c_1 -| tenv_1
  [ 3]       → Check_command
  [ 3]           []
  [ 3]           int x = 5
  [ 4]         → Check_command/skip
  [ 4]           | -- if command matches `SKIP`
  [ 4]         ← Check_command/skip [fail]
  [ 4]         → Check_command/decl
  [ 4]           | -- if command matches `% % = %`
  [ 4]           | -- let t x = e = command
  [ 4]           | -- rel Check_expr: tenv |- e : type
  [ 4]         → Check_expr
  [ 4]             []
  [ 4]             5
  [ 5]           → Check_expr/num
  [ 5]             | -- if expr <: literal
  [ 5]             | -- let literal = expr as literal
  [ 5]             | -- if literal matches ``NUM %`
  [ 5]             | -- let i = literal
  [ 5]           ← Check_expr/num [ok]
  [ 4]         ← Check_expr [ok]
  [ 4]             int
  [ 4]           | -- if (type = t)
  [ 4]         ← Check_command/decl [ok]
  [ 3]       ← Check_command [ok]
  [ 3]           [ x -> int ]
  [ 3]         | -- rel Check_command: tenv_1 |- c_2 -| tenv_2
  [ 3]       → Check_command
  [ 3]           [ x -> int ]
  [ 3]           bool y = x <= 10
  [ 4]         → Check_command/skip
  [ 4]           | -- if command matches `SKIP`
  [ 4]         ← Check_command/skip [fail]
  [ 4]         → Check_command/decl
  [ 4]           | -- if command matches `% % = %`
  [ 4]           | -- let t x = e = command
  [ 4]           | -- rel Check_expr: tenv |- e : type
  [ 4]         → Check_expr
  [ 4]             [ x -> int ]
  [ 4]             x <= 10
  [ 5]           → Check_expr/num
  [ 5]             | -- if expr <: literal
  [ 5]           ← Check_expr/num [fail]
  [ 5]           → Check_expr/boollit
  [ 5]             | -- if expr <: literal
  [ 5]           ← Check_expr/boollit [fail]
  [ 5]           → Check_expr/id
  [ 5]             | -- if expr <: id
  [ 5]           ← Check_expr/id [fail]
  [ 5]           → Check_expr/not
  [ 5]             | -- if expr matches `! %`
  [ 5]           ← Check_expr/not [fail]
  [ 5]           → Check_expr/add
  [ 5]             | -- if expr matches `% + %`
  [ 5]           ← Check_expr/add [fail]
  [ 5]           → Check_expr/leq
  [ 5]             | -- if expr matches `% <= %`
  [ 5]             | -- let e_l <= e_r = expr
  [ 5]             | -- rel Check_expr: tenv |- e_l : type
  [ 5]           → Check_expr
  [ 5]               [ x -> int ]
  [ 5]               x
  [ 6]             → Check_expr/num
  [ 6]               | -- if expr <: literal
  [ 6]             ← Check_expr/num [fail]
  [ 6]             → Check_expr/boollit
  [ 6]               | -- if expr <: literal
  [ 6]             ← Check_expr/boollit [fail]
  [ 6]             → Check_expr/id
  [ 6]               | -- if expr <: id
  [ 6]               | -- let x = expr as id
  [ 6]               | -- let type?{type <- type?} = $lookup<id, type>(tenv, x)
  [ 6]             → $lookup
  [ 6]                 [ x -> int ]
  [ 6]                 x
  [ 7]               → $lookup/0
  [ 7]                 | -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches []
  [ 7]               ← $lookup
  [ 7]               → $lookup/1
  [ 7]                 | -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _
  [ 7]                 | -- let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} = pair<K, V>*{pair<K, V> <- pair<K, V>*}
  [ 7]                 | -- if (K_h = K_query)
  [ 7]               ← $lookup
  [ 6]             ← $lookup
  [ 6]                 Some(int)
  [ 6]               | -- if type?{type <- type?} matches (_)
  [ 6]               | -- let ?(t) = type?{type <- type?}
  [ 6]             ← Check_expr/id [ok]
  [ 5]           ← Check_expr [ok]
  [ 5]               int
  [ 5]             | -- if type matches `INT`
  [ 5]             | -- rel Check_expr: tenv |- e_r : type'
  [ 5]           → Check_expr
  [ 5]               [ x -> int ]
  [ 5]               10
  [ 6]             → Check_expr/num
  [ 6]               | -- if expr <: literal
  [ 6]               | -- let literal = expr as literal
  [ 6]               | -- if literal matches ``NUM %`
  [ 6]               | -- let i = literal
  [ 6]             ← Check_expr/num [ok]
  [ 5]           ← Check_expr [ok]
  [ 5]               int
  [ 5]             | -- if type' matches `INT`
  [ 5]           ← Check_expr/leq [ok]
  [ 4]         ← Check_expr [ok]
  [ 4]             bool
  [ 4]           | -- if (type = t)
  [ 4]         ← Check_command/decl [ok]
  [ 3]       ← Check_command [ok]
  [ 3]           [ y -> bool, x -> int ]
  [ 3]       ← Check_command/seq [ok]
  [ 2]     ← Check_command [ok]
  [ 2]         [ y -> bool, x -> int ]
  [ 2]     ← Check_prog/ [ok]
  [ 1]   ← Check_prog [ok]
  [ 1]     | -- rel Eval_prog: |- command -| env
  [ 1]   → Eval_prog
  [ 1]       int x = 5 ; bool y = x <= 10
  [ 2]     → Eval_prog/
  [ 2]       | -- rel Eval_command: [] |- command -| env
  [ 2]     → Eval_command
  [ 2]         []
  [ 2]         int x = 5 ; bool y = x <= 10
  [ 3]       → Eval_command/skip
  [ 3]         | -- if command matches `SKIP`
  [ 3]       ← Eval_command/skip [fail]
  [ 3]       → Eval_command/decl
  [ 3]         | -- if command matches `% % = %`
  [ 3]       ← Eval_command/decl [fail]
  [ 3]       → Eval_command/assign
  [ 3]         | -- if command matches `% = %`
  [ 3]       ← Eval_command/assign [fail]
  [ 3]       → Eval_command/ite-true
  [ 3]         | -- if command matches `IF % THEN % ELSE % END`
  [ 3]       ← Eval_command/ite-true [fail]
  [ 3]       → Eval_command/ite-false
  [ 3]         | -- if command matches `IF % THEN % ELSE % END`
  [ 3]       ← Eval_command/ite-false [fail]
  [ 3]       → Eval_command/while-false
  [ 3]         | -- if command matches `WHILE % DO % END`
  [ 3]       ← Eval_command/while-false [fail]
  [ 3]       → Eval_command/while-true
  [ 3]         | -- if command matches `WHILE % DO % END`
  [ 3]       ← Eval_command/while-true [fail]
  [ 3]       → Eval_command/seq
  [ 3]         | -- if command matches `% ; %`
  [ 3]         | -- let c_1 ; c_2 = command
  [ 3]         | -- rel Eval_command: env |- c_1 -| env_1
  [ 3]       → Eval_command
  [ 3]           []
  [ 3]           int x = 5
  [ 4]         → Eval_command/skip
  [ 4]           | -- if command matches `SKIP`
  [ 4]         ← Eval_command/skip [fail]
  [ 4]         → Eval_command/decl
  [ 4]           | -- if command matches `% % = %`
  [ 4]           | -- let t x = e = command
  [ 4]           | -- rel Eval_expr: env |- e ==> v
  [ 4]         → Eval_expr
  [ 4]             []
  [ 4]             5
  [ 5]           → Eval_expr/num
  [ 5]             | -- if expr <: literal
  [ 5]             | -- let literal = expr as literal
  [ 5]             | -- if literal matches ``NUM %`
  [ 5]             | -- let i = literal
  [ 5]           ← Eval_expr/num [ok]
  [ 4]         ← Eval_expr [ok]
  [ 4]             5
  [ 4]         ← Eval_command/decl [ok]
  [ 3]       ← Eval_command [ok]
  [ 3]           [ x -> 5 ]
  [ 3]         | -- rel Eval_command: env_1 |- c_2 -| env_2
  [ 3]       → Eval_command
  [ 3]           [ x -> 5 ]
  [ 3]           bool y = x <= 10
  [ 4]         → Eval_command/skip
  [ 4]           | -- if command matches `SKIP`
  [ 4]         ← Eval_command/skip [fail]
  [ 4]         → Eval_command/decl
  [ 4]           | -- if command matches `% % = %`
  [ 4]           | -- let t x = e = command
  [ 4]           | -- rel Eval_expr: env |- e ==> v
  [ 4]         → Eval_expr
  [ 4]             [ x -> 5 ]
  [ 4]             x <= 10
  [ 5]           → Eval_expr/num
  [ 5]             | -- if expr <: literal
  [ 5]           ← Eval_expr/num [fail]
  [ 5]           → Eval_expr/boollit
  [ 5]             | -- if expr <: literal
  [ 5]           ← Eval_expr/boollit [fail]
  [ 5]           → Eval_expr/id
  [ 5]             | -- if expr <: id
  [ 5]           ← Eval_expr/id [fail]
  [ 5]           → Eval_expr/not
  [ 5]             | -- if expr matches `! %`
  [ 5]           ← Eval_expr/not [fail]
  [ 5]           → Eval_expr/add
  [ 5]             | -- if expr matches `% + %`
  [ 5]           ← Eval_expr/add [fail]
  [ 5]           → Eval_expr/leq
  [ 5]             | -- if expr matches `% <= %`
  [ 5]             | -- let e_l <= e_r = expr
  [ 5]             | -- rel Eval_expr: env |- e_l ==> literal
  [ 5]           → Eval_expr
  [ 5]               [ x -> 5 ]
  [ 5]               x
  [ 6]             → Eval_expr/num
  [ 6]               | -- if expr <: literal
  [ 6]             ← Eval_expr/num [fail]
  [ 6]             → Eval_expr/boollit
  [ 6]               | -- if expr <: literal
  [ 6]             ← Eval_expr/boollit [fail]
  [ 6]             → Eval_expr/id
  [ 6]               | -- if expr <: id
  [ 6]               | -- let x = expr as id
  [ 6]               | -- let value?{value <- value?} = $lookup<id, value>(env, x)
  [ 6]             → $lookup
  [ 6]                 [ x -> 5 ]
  [ 6]                 x
  [ 7]               → $lookup/0
  [ 7]                 | -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches []
  [ 7]               ← $lookup
  [ 7]               → $lookup/1
  [ 7]                 | -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _
  [ 7]                 | -- let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} = pair<K, V>*{pair<K, V> <- pair<K, V>*}
  [ 7]                 | -- if (K_h = K_query)
  [ 7]               ← $lookup
  [ 6]             ← $lookup
  [ 6]                 Some(5)
  [ 6]               | -- if value?{value <- value?} matches (_)
  [ 6]               | -- let ?(v) = value?{value <- value?}
  [ 6]             ← Eval_expr/id [ok]
  [ 5]           ← Eval_expr [ok]
  [ 5]               5
  [ 5]             | -- if literal matches ``NUM %`
  [ 5]             | -- let i_l = literal
  [ 5]             | -- rel Eval_expr: env |- e_r ==> literal'
  [ 5]           → Eval_expr
  [ 5]               [ x -> 5 ]
  [ 5]               10
  [ 6]             → Eval_expr/num
  [ 6]               | -- if expr <: literal
  [ 6]               | -- let literal = expr as literal
  [ 6]               | -- if literal matches ``NUM %`
  [ 6]               | -- let i = literal
  [ 6]             ← Eval_expr/num [ok]
  [ 5]           ← Eval_expr [ok]
  [ 5]               10
  [ 5]             | -- if literal' matches ``NUM %`
  [ 5]             | -- let i_r = literal'
  [ 5]             | -- let b = (i_l <= i_r)
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
