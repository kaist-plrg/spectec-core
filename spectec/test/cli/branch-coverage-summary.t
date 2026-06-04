impty CLI: branch-coverage summary against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Summary branch coverage reports only the uncovered rules and clauses, under the
IL interpreter:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --branch-coverage.level summary
  
  === Branch Coverage ===
  
  Rules: 13/31 (41.94%)
  
  Uncovered rules:
    Check_command/skip
    Check_command/assign
    Check_command/ite
    Check_command/while
    Check_expr/boollit
    Check_expr/not
    Check_expr/add
    Check_expr/and
    Eval_command/skip
    Eval_command/assign
    Eval_command/ite-true
    Eval_command/ite-false
    Eval_command/while-false
    Eval_command/while-true
    Eval_expr/boollit
    Eval_expr/not
    Eval_expr/add
    Eval_expr/and
  
  Clauses: 1/3 (33.33%)
  
  Uncovered clauses:
    $lookup/0
    $lookup/2
  [
    y -> true,
    x -> 5
  ]
