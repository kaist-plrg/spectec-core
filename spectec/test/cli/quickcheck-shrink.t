impty quickcheck: the shape of the shrunk counterexample.

Each fixture drops a type constraint from one operator, so an ill-typed operand
typechecks but gets stuck in evaluation. The shrinker reports it inside a
declaration, collapsing any loop the generator built around it.

A bug in '+' (right operand unconstrained). A sum is INT-typed, so it fits only in
a declaration:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-add.spectec --num-tests 500 --color never
  Type_safety: falsified after 76 tests
    counterexample   prog: int x0 = -10 + true
  
  Type_preservation: passed 500 tests

A bug in '!' (operand unconstrained). A negation is BOOL-typed, so the generator
can build a loop, which the shrinker collapses to a declaration:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-not.spectec --num-tests 500 --color never
  Type_safety: falsified after 9 tests
    counterexample   prog: bool x = ! -2
  
  Type_preservation: passed 500 tests

