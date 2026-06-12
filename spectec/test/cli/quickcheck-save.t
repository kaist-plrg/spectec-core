impty quickcheck --save-dir: save counterexamples as runnable .imp programs.

A sound spec saves nothing:

  $ spectec impty quickcheck --spec ../../specs/impty/base/spec.spectec --num-tests 50 --save-dir sound --color never
  Type_safety: passed 50 tests
  Type_preservation: passed 50 tests

Both counterexamples are saved. The Type_safety one is already a program; the
Type_preservation one is a typing triple (a value environment, a typing context, and
an expression), reconstructed into an equivalent program: the expression bound
under its static type, preceded by a declaration for each environment binding
(here the environment is empty):

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-preservation.spectec --num-tests 500 --save-dir buggy --color never
  Type_safety: falsified after 6 tests
    counterexample   prog: while 0 <= 0 do skip end
    saved            buggy/counter_Type_safety.imp
  Type_preservation: falsified after 14 tests
    counterexample   tenv: [], env: [], expr: 2 <= 2
    program          bool result = 2 <= 2
    saved            buggy/counter_Type_preservation.imp

  $ cat buggy/counter_Type_safety.imp
  // quickcheck counterexample for Type_safety
  while 0 <= 0 do skip end

  $ cat buggy/counter_Type_preservation.imp
  // quickcheck counterexample for Type_preservation
  bool result = 2 <= 2

The reconstructed program typechecks and runs to completion. Unlike a Type_safety
counterexample, running it does not surface the bug: `result` is declared bool yet
evaluation binds it to a number. The Type_preservation property is what catches that.

  $ spectec impty typecheck --spec ../../testdata/quickcheck/buggy-preservation.spectec -p buggy/counter_Type_preservation.imp --color never
  Typecheck succeeded

  $ spectec impty eval --spec ../../testdata/quickcheck/buggy-preservation.spectec -p buggy/counter_Type_preservation.imp --color never
  [
    result -> 2
  ]

The saved counterexample is a runnable test. Under a spec that forgets
to constrain the right operand of a comparison, quickcheck finds a
program that typechecks but gets stuck in eval:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-leq.spectec --num-tests 100 --save-dir leq --color never
  Type_safety: falsified after 67 tests
    counterexample   prog: while 8 <= false do skip end
    saved            leq/counter_Type_safety.imp

Re-running finds the same counterexample; rather than write a duplicate, it
reports the path of the file that already holds it:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-leq.spectec --num-tests 100 --save-dir leq --color never
  Type_safety: falsified after 67 tests
    counterexample   prog: while 8 <= false do skip end
    saved            leq/counter_Type_safety.imp (already saved)

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

With --generalize the report also prints an abstract generalized form. The
saved file is the concrete, runnable counterexample, never the generalization:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-leq.spectec --num-tests 100 --generalize --save-dir gen --color never
  Type_safety: falsified after 67 tests
    counterexample   prog: while 8 <= false do skip end
    generalized      prog: while [int] <= [bool] do [command] end
    saved            gen/counter_Type_safety.imp

  $ cat gen/counter_Type_safety.imp
  // quickcheck counterexample for Type_safety
  while 8 <= false do skip end
