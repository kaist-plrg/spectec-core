impty CLI: tree conclusion against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The conclusion level renders each relation's conclusion as an inference figure:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --tree.level conclusion
  Run_prog:
  |- int x = 5 ; bool y = x <= 10 -| [ y -> true, x -> 5 ]
  ────────────────────────────────────────────────────────
  -- Check_prog:
     |- int x = 5 ; bool y = x <= 10
     ───────────────────────────────
     -- Check_command/seq:
        [] |- int x = 5 ; bool y = x <= 10 -| [ y -> bool, x -> int ]
        ─────────────────────────────────────────────────────────────
        -- Check_command/decl:
           [] |- int x = 5 -| [ x -> int ]
           ───────────────────────────────
           -- Check_expr/num:
              [] |- 5 : int
              ─────────────
        -- Check_command/decl:
           [ x -> int ] |- bool y = x <= 10 -| [ y -> bool, x -> int ]
           ───────────────────────────────────────────────────────────
           -- Check_expr/leq:
              [ x -> int ] |- x <= 10 : bool
              ──────────────────────────────
              -- Check_expr/id:
                 [ x -> int ] |- x : int
                 ───────────────────────
              -- Check_expr/num:
                 [ x -> int ] |- 10 : int
                 ────────────────────────
  -- Eval_prog:
     |- int x = 5 ; bool y = x <= 10 -| [ y -> true, x -> 5 ]
     ────────────────────────────────────────────────────────
     -- Eval_command/seq:
        [] |- int x = 5 ; bool y = x <= 10 -| [ y -> true, x -> 5 ]
        ───────────────────────────────────────────────────────────
        -- Eval_command/decl:
           [] |- int x = 5 -| [ x -> 5 ]
           ─────────────────────────────
           -- Eval_expr/num:
              [] |- 5 ==> 5
              ─────────────
        -- Eval_command/decl:
           [ x -> 5 ] |- bool y = x <= 10 -| [ y -> true, x -> 5 ]
           ───────────────────────────────────────────────────────
           -- Eval_expr/leq:
              [ x -> 5 ] |- x <= 10 ==> true
              ──────────────────────────────
              -- Eval_expr/id:
                 [ x -> 5 ] |- x ==> 5
                 ─────────────────────
              -- Eval_expr/num:
                 [ x -> 5 ] |- 10 ==> 10
                 ───────────────────────
  [
    y -> true,
    x -> 5
  ]

On failure the conclusion level renders the partial derivation with values,
crossing the rule that could not be completed and leaving its output slot blank:

  $ ERR=../../testdata/interp/impty/base/_errors_not_int.imp
  $ spectec impty eval --spec $SPEC -p $ERR --color never --tree.level conclusion 2>/dev/null
  Run_prog:
  |- bool b = ! 5 -| ?
  ────────────────────
  ✗  Check_prog:
     |- bool b = ! 5
     ───────────────
     ✗  Check_command/decl:
        [] |- bool b = ! 5 -| ?
        ───────────────────────
        ✗  Check_expr/not:
           [] |- ! 5 : ?
           ─────────────
           -- Check_expr/num:
              [] |- 5 : int
              ─────────────
  [1]
