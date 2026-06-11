impty fuzz: save quickcheck counterexamples as runnable .imp programs.

A sound spec saves nothing:

  $ spectec impty fuzz --spec ../../specs/impty/base/spec.spectec --num-tests 50 --out-dir sound --color never
  [Quickcheck Type_safety: Test]
  OK, passed 50 samples.
  [Quickcheck Preservation: Test]
  OK, passed 50 samples.

A program counterexample (Type_safety) is saved; a non-program one
(Preservation, a typing triple) is skipped, not crashed on:

  $ spectec impty fuzz --spec ../../testdata/quickcheck/buggy-preservation.spectec --num-tests 500 --out-dir buggy --color never
  [Quickcheck Type_safety: Test]
  Falsifiable, after 6 tests:
    prog=while +0 <= +0 do skip end
  [Quickcheck Preservation: Test]
  Falsifiable, after 14 tests:
    env=[], tenv=[], expr=+2 <= +2
    saved counterexample to buggy/counter_Type_safety.imp
  fuzz: Preservation: counterexample is not a program; not saved

  $ cat buggy/counter_Type_safety.imp
  // quickcheck counterexample for Type_safety
  while 0 <= 0 do skip end

The saved counterexample is a runnable test. Under a spec that forgets
to constrain the right operand of a comparison, fuzz finds a program
that typechecks but gets stuck in eval:

  $ spectec impty fuzz --spec ../../testdata/quickcheck/buggy-leq.spectec --num-tests 100 --out-dir leq --color never
  [Quickcheck Type_safety: Test]
  Falsifiable, after 67 tests:
    prog=while +8 <= false do skip end
    saved counterexample to leq/counter_Type_safety.imp

  $ cat leq/counter_Type_safety.imp
  // quickcheck counterexample for Type_safety
  while 8 <= false do skip end

  $ spectec impty typecheck --spec ../../testdata/quickcheck/buggy-leq.spectec -p leq/counter_Type_safety.imp --color never
  Typecheck succeeded

  $ spectec impty eval --spec ../../testdata/quickcheck/buggy-leq.spectec -p leq/counter_Type_safety.imp --color never
  error: invocation of relation Run_prog failed
    --> ../../testdata/quickcheck/buggy-leq.spectec:260:6
      |
  260 |   -- Eval_prog: |- command -| env
      |      ^^^^^^^^^
      |
      | source: il-interp
      |
      | trace:
      | application of rule Run_prog/ failed
      | └── ../../testdata/quickcheck/buggy-leq.spectec:260:6-260:15:
      |     invocation of relation Eval_prog failed
      |     └── ../../testdata/quickcheck/buggy-leq.spectec:260:6-260:15:
      |         application of rule Eval_prog/ failed
      |         └── ../../testdata/quickcheck/buggy-leq.spectec:249:6-249:18:
      |             invocation of relation Eval_command failed
      |             ├── ../../testdata/quickcheck/buggy-leq.spectec:249:6-249:18:
      |             │   application of rule Eval_command/while-false failed
      |             │   └── ../../testdata/quickcheck/buggy-leq.spectec:227:6-227:15:
      |             │       invocation of relation Eval_expr failed
      |             │       └── ../../testdata/quickcheck/buggy-leq.spectec:227:6-227:15:
      |             │           application of rule Eval_expr/leq failed
      |             │           └── ../../testdata/quickcheck/buggy-leq.spectec:188:33-188:41:
      |             │               condition literal' matches `_NUM %` was not met
      |             └── ../../testdata/quickcheck/buggy-leq.spectec:249:6-249:18:
      |                 application of rule Eval_command/while-true failed
      |                 └── ../../testdata/quickcheck/buggy-leq.spectec:231:6-231:15:
      |                     invocation of relation Eval_expr failed
      |                     └── ../../testdata/quickcheck/buggy-leq.spectec:231:6-231:15:
      |                         application of rule Eval_expr/leq failed
      |                         └── ../../testdata/quickcheck/buggy-leq.spectec:188:33-188:41:
      |                             condition literal' matches `_NUM %` was not met
  [1]
