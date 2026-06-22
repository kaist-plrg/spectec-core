impty CLI: tree premise against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

The premise level adds each rule's premises below its conclusion, rendered in
author syntax with runtime values substituted at variable leaves. It is IL only.

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --tree.level premise
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
                 -- if $lookup<id, type>([ x -> int ], x) = int
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
                 -- if $lookup<id, value>([ x -> 5 ], x) = 5
              -- Eval_expr/num:
                 [ x -> 5 ] |- 10 ==> 10
                 ───────────────────────
              -- if true = $(5 <= 10)
  [
    y -> true,
    x -> 5
  ]

On failure the premise level still renders the partial derivation, crossing the
premise that defeated the rule. With the tree shown, the diagnostic keeps only
its headline:

  $ ERR=../../testdata/interp/impty/base/_errors_not_int.imp
  $ spectec impty eval --spec $SPEC -p $ERR --color never --tree.level premise
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
           ✗  Check_expr: [] |- 5 : BOOL
  error: invocation of relation Run_prog failed
    --> ../../specs/impty/base/spec.spectec:256:6
      |
  256 |   -- Check_prog: |- command
      |      ^^^^^^^^^^
      |
      | source: il-interp
  [1]

The SL interpreter reports no premises, so the premise level is refused rather
than silently falling back to a coarser view:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --sl --tree.level premise
  error: Instrumentation handlers incompatible with SL mode: tree (IL only)
  
    source: config
  [1]
