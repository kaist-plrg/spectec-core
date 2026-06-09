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
    prog=bool x0 = ! +0 <= +1
  [Quickcheck Preservation: Test]
  Falsifiable, after 14 tests:
    env=[], tenv=[], expr=+2 <= +2
    saved counterexample to buggy/counter_Type_safety.imp
  fuzz: Preservation: counterexample is not a program; not saved

  $ cat buggy/counter_Type_safety.imp
  // quickcheck counterexample for Type_safety
  bool x0 = !(0 <= 1)
