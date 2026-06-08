impty CLI: premise-coverage summary against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Premise coverage is IL-only; run it on the typechecker. Summary lists only the
premises that never succeeded:

  $ spectec impty typecheck --spec $SPEC -p $HELLO --color never --premise-coverage.level summary
  
  === IL Node Coverage ===
  
  IL Premises: 35/151 attempted (23.18%), 27/151 succeeded (17.88%)
  38 rel premises
  62 if-premises: succeeded 12/62 (19.35%), failed 11/62 (17.74%), neither 42/62 (67.74%), total 23/124 (18.55%)
  
  Never succeeded:
    lookup/clause/0:
      if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches []
    lookup/clause/2:
      if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _
    lookup/clause/2:
      let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} = pair<K, V>*{pair<K, V> <- pair<K, V>*}
    lookup/clause/2:
      otherwise
    Check_expr/boollit:
      if expr <: literal
    Check_expr/boollit:
      let literal = expr as literal
    Check_expr/boollit:
      if literal matches ``BOOL %`
    Check_expr/boollit:
      let b = literal
    Check_expr/not:
      if expr matches `! %`
    Check_expr/not:
      let ! e = expr
    Check_expr/not:
      rel Check_expr: tenv |- e : type
    Check_expr/not:
      if type matches `BOOL`
    Check_expr/add:
      if expr matches `% + %`
    Check_expr/add:
      let e_l + e_r = expr
    Check_expr/add:
      rel Check_expr: tenv |- e_l : type
    Check_expr/add:
      if type matches `INT`
    Check_expr/add:
      rel Check_expr: tenv |- e_r : type'
    Check_expr/add:
      if type' matches `INT`
    Check_expr/and:
      if expr matches `% && %`
    Check_expr/and:
      let e_l && e_r = expr
    Check_expr/and:
      rel Check_expr: tenv |- e_l : type
    Check_expr/and:
      if type matches `BOOL`
    Check_expr/and:
      rel Check_expr: tenv |- e_r : type'
    Check_expr/and:
      if type' matches `BOOL`
    Check_command/skip:
      if command matches `SKIP`
    Check_command/assign:
      if command matches `% = %`
    Check_command/assign:
      let x = e = command
    Check_command/assign:
      rel Check_expr: tenv |- e : t
    Check_command/assign:
      if ($lookup<id, type>(tenv, x) = ?(t))
    Check_command/ite:
      if command matches `IF % THEN % ELSE % END`
    Check_command/ite:
      let if e then c_1 else c_2 end = command
    Check_command/ite:
      rel Check_expr: tenv |- e : type
    Check_command/ite:
      if type matches `BOOL`
    Check_command/ite:
      rel Check_command: tenv |- c_1 -| tenv_1
    Check_command/ite:
      rel Check_command: tenv |- c_2 -| tenv_2
    Check_command/while:
      if command matches `WHILE % DO % END`
    Check_command/while:
      let while e do c end = command
    Check_command/while:
      rel Check_expr: tenv |- e : type
    Check_command/while:
      if type matches `BOOL`
    Check_command/while:
      rel Check_command: tenv |- c -| tenv_1
    Eval_expr/num:
      if expr <: literal
    Eval_expr/num:
      let literal = expr as literal
    Eval_expr/num:
      if literal matches ``NUM %`
    Eval_expr/num:
      let i = literal
    Eval_expr/boollit:
      if expr <: literal
    Eval_expr/boollit:
      let literal = expr as literal
    Eval_expr/boollit:
      if literal matches ``BOOL %`
    Eval_expr/boollit:
      let b = literal
    Eval_expr/id:
      if expr <: id
    Eval_expr/id:
      let x = expr as id
    Eval_expr/id:
      let value?{value <- value?} = $lookup<id, value>(env, x)
    Eval_expr/id:
      if value?{value <- value?} matches (_)
    Eval_expr/id:
      let ?(v) = value?{value <- value?}
    Eval_expr/not:
      if expr matches `! %`
    Eval_expr/not:
      let ! e = expr
    Eval_expr/not:
      rel Eval_expr: env |- e ==> literal
    Eval_expr/not:
      if literal matches ``BOOL %`
    Eval_expr/not:
      let b_e = literal
    Eval_expr/not:
      let b = ~b_e
    Eval_expr/add:
      if expr matches `% + %`
    Eval_expr/add:
      let e_l + e_r = expr
    Eval_expr/add:
      rel Eval_expr: env |- e_l ==> literal
    Eval_expr/add:
      if literal matches ``NUM %`
    Eval_expr/add:
      let i_l = literal
    Eval_expr/add:
      rel Eval_expr: env |- e_r ==> literal'
    Eval_expr/add:
      if literal' matches ``NUM %`
    Eval_expr/add:
      let i_r = literal'
    Eval_expr/add:
      let i = (i_l + i_r)
    Eval_expr/leq:
      if expr matches `% <= %`
    Eval_expr/leq:
      let e_l <= e_r = expr
    Eval_expr/leq:
      rel Eval_expr: env |- e_l ==> literal
    Eval_expr/leq:
      if literal matches ``NUM %`
    Eval_expr/leq:
      let i_l = literal
    Eval_expr/leq:
      rel Eval_expr: env |- e_r ==> literal'
    Eval_expr/leq:
      if literal' matches ``NUM %`
    Eval_expr/leq:
      let i_r = literal'
    Eval_expr/leq:
      let b = (i_l <= i_r)
    Eval_expr/and:
      if expr matches `% && %`
    Eval_expr/and:
      let e_l && e_r = expr
    Eval_expr/and:
      rel Eval_expr: env |- e_l ==> literal
    Eval_expr/and:
      if literal matches ``BOOL %`
    Eval_expr/and:
      let b_l = literal
    Eval_expr/and:
      rel Eval_expr: env |- e_r ==> literal'
    Eval_expr/and:
      if literal' matches ``BOOL %`
    Eval_expr/and:
      let b_r = literal'
    Eval_expr/and:
      let b = (b_l /\ b_r)
    Eval_command/skip:
      if command matches `SKIP`
    Eval_command/decl:
      if command matches `% % = %`
    Eval_command/decl:
      let t x = e = command
    Eval_command/decl:
      rel Eval_expr: env |- e ==> v
    Eval_command/assign:
      if command matches `% = %`
    Eval_command/assign:
      let x = e = command
    Eval_command/assign:
      rel Eval_expr: env |- e ==> v
    Eval_command/ite-true:
      if command matches `IF % THEN % ELSE % END`
    Eval_command/ite-true:
      let if e then c_1 else c_2 end = command
    Eval_command/ite-true:
      rel Eval_expr: env |- e ==> literal
    Eval_command/ite-true:
      if (literal = true)
    Eval_command/ite-true:
      rel Eval_command: env |- c_1 -| env_1
    Eval_command/ite-false:
      if command matches `IF % THEN % ELSE % END`
    Eval_command/ite-false:
      let if e then c_1 else c_2 end = command
    Eval_command/ite-false:
      rel Eval_expr: env |- e ==> literal
    Eval_command/ite-false:
      if (literal = false)
    Eval_command/ite-false:
      rel Eval_command: env |- c_2 -| env_2
    Eval_command/while-false:
      if command matches `WHILE % DO % END`
    Eval_command/while-false:
      let while e do c end = command
    Eval_command/while-false:
      rel Eval_expr: env |- e ==> literal
    Eval_command/while-false:
      if (literal = false)
    Eval_command/while-true:
      if command matches `WHILE % DO % END`
    Eval_command/while-true:
      let while e do c end = command
    Eval_command/while-true:
      rel Eval_expr: env |- e ==> literal
    Eval_command/while-true:
      if (literal = true)
    Eval_command/while-true:
      rel Eval_command: env |- c -| env_1
    Eval_command/while-true:
      rel Eval_command: env_1 |- while e do c end -| env_2
    Eval_command/seq:
      if command matches `% ; %`
    Eval_command/seq:
      let c_1 ; c_2 = command
    Eval_command/seq:
      rel Eval_command: env |- c_1 -| env_1
    Eval_command/seq:
      rel Eval_command: env_1 |- c_2 -| env_2
    Eval_prog/:
      rel Eval_command: [] |- command -| env
    Run_prog/:
      if Check_prog: |- command holds
    Run_prog/:
      rel Eval_prog: |- command -| env
    Check_value/num:
      if literal matches ``NUM %`
    Check_value/num:
      let int = literal
    Check_value/boollit:
      if literal matches ``BOOL %`
    Check_value/boollit:
      let bool = literal
  Typecheck succeeded
