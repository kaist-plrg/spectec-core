impty quickcheck: the shape of the shrunk counterexample.

Each fixture drops a type constraint from one operator, so an ill-typed operand
typechecks but gets stuck in evaluation. The result type decides where that
expression can sit, and so the shape of the shrunk counterexample.

A bug in '+' (right operand unconstrained). A sum is INT-typed, so it fits only in
a declaration:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-add.spectec --num-tests 500 --color never
  Type_safety: falsified after 76 tests
    counterexample   prog: int x0 = -10 + true
  
  Type_preservation: passed 500 tests

A bug in '!' (operand unconstrained). A negation is BOOL-typed, so it fits in a
loop condition:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-not.spectec --num-tests 500 --color never
  Type_safety: falsified after 9 tests
    counterexample   prog: while ! -2 do bool x0 = true end
  
  Type_preservation: passed 500 tests

