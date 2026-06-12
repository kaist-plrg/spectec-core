impty CLI: branch-coverage full against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Full branch coverage emits every relation and function with coverage
annotations, not just the uncovered ones:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --branch-coverage.level full
  
  === Branch Coverage ===
  
  -- Relations --
  
  relation Check_command: (2/6 = 33.33%)
    ####  rule assign
       2  rule decl
    ####  rule ite
       1  rule seq
    ####  rule skip
    ####  rule while
  
  relation Check_expr: (3/7 = 42.86%)
    ####  rule add
    ####  rule and
    ####  rule boollit
       1  rule id
       1  rule leq
    ####  rule not
       2  rule num
  
  relation Check_prog: (1/1 = 100.00%)
       1  rule 
  
  relation Eval_command: (2/8 = 25.00%)
    ####  rule assign
       2  rule decl
    ####  rule ite-false
    ####  rule ite-true
       1  rule seq
    ####  rule skip
    ####  rule while-false
    ####  rule while-true
  
  relation Eval_expr: (3/7 = 42.86%)
    ####  rule add
    ####  rule and
    ####  rule boollit
       1  rule id
       1  rule leq
    ####  rule not
       2  rule num
  
  relation Eval_prog: (1/1 = 100.00%)
       1  rule 
  
  relation Has_type: (0/2 = 0.00%)
    ####  rule boollit
    ####  rule num
  
  relation Run_prog: (1/1 = 100.00%)
       1  rule 
  
  -- Functions --
  
  def $lookup: (1/3 = 33.33%)
    ####  clause 0
       2  clause 1
    ####  clause 2
  
  [
    y -> true,
    x -> 5
  ]
