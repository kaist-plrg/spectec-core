impty quickcheck: property-based testing output.

Quickcheck is seeded deterministically, so a fixed --num-tests yields the same
counterexample on every run. These cases pin the four output shapes: the flag
surface, a passing run, a falsifiable run, and a generalized counterexample.

The flag surface, including the default sample count:

  $ spectec impty quickcheck --help
  run quickcheck properties declared in an impty spec
  
    spectec impty quickcheck 
  
  === flags ===
  
    [--branch-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--branch-coverage.output FILE]
                               . output destination file
    [--color WHEN]             . colorize diagnostics: auto|always|never (default:
                                 auto)
    [--generalize]             . generalize counterexamples after shrinking
    [--instruction-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--instruction-coverage.output FILE]
                               . output destination file
    [--max-steps N]            . max steps per relation evaluation (default 100)
    [--num-tests N]            . number of test cases to generate (default 500)
    [--premise-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--premise-coverage.output FILE]
                               . output destination file
    [--profile.output FILE]    . output destination file
    [--save-dir DIR]           . save program counterexamples as .imp files under
                                 DIR
    [--spec FILES] ...         . spec files; mutually exclusive with --spec-dir
    [--spec-dir DIR]           . directory of .spectec files, collected
                                 recursively; mutually exclusive with --spec
    [--trace.level LEVEL]      . verbosity level: summary|rules|inputs|full
    [--trace.output FILE]      . output destination file
    [--tree.level LEVEL]       . verbosity level: rule|conclusion|premise
    [--tree.output FILE]       . output destination file
    [-help], -?                . print this help text and exit
  

A sound spec passes both properties: every generated program that typechecks
also evaluates (Type_safety), and every well-typed expression evaluates to a
value of its static type (Preservation).

  $ spectec impty quickcheck --spec ../../specs/impty/base/spec.spectec --num-tests 50 --color never
  [Quickcheck Type_safety: Test]
  OK, passed 50 samples.
  [Quickcheck Preservation: Test]
  OK, passed 50 samples.

The buggy-leq fixture types `e <= e'` without constraining the right operand,
so a program like `1 <= false` typechecks but gets stuck in evaluation:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-leq.spectec --num-tests 500 --color never
  [Quickcheck Type_safety: Test]
  Falsifiable, after 67 tests:
    prog=while 8 <= false do skip end

With --generalize, the shrunk counterexample is widened to the family of
programs that exhibit the bug:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-leq.spectec --num-tests 500 --generalize --color never
  [Quickcheck Type_safety: Test]
  Falsifiable, after 67 tests:
    prog=while 8 <= false do skip end
    (Generalized)
    prog=while [int] <= [bool] do [command] end

The buggy-preservation fixture evaluates `e <= e'` to a NUM while still typing it
as BOOL, so a well-typed expression evaluates to a value of the wrong type. The
Preservation property catches it directly with the offending expression:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-preservation.spectec --num-tests 500 --color never
  [Quickcheck Type_safety: Test]
  Falsifiable, after 6 tests:
    prog=while 0 <= 0 do skip end
  [Quickcheck Preservation: Test]
  Falsifiable, after 14 tests:
    env=[], tenv=[], expr=2 <= 2

Generalizing widens the expression but skips the map-typed env and tenv, which
have no generator:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/buggy-preservation.spectec --num-tests 500 --generalize --color never
  [Quickcheck Type_safety: Test]
  Falsifiable, after 6 tests:
    prog=while 0 <= 0 do skip end
    (Generalized)
    prog=while [int] <= [int] do [command] end
  [Quickcheck Preservation: Test]
  Falsifiable, after 14 tests:
    env=[], tenv=[], expr=2 <= 2
    (Generalized)
    env=[], tenv=[], expr=[int] <= [int]

The preservation-compat fixture spells out the environment/context agreement with
let- and iteration-premises: it destructures `env` and `tenv` into key/value
sequences and requires their keys to match pointwise. Evaluating it exercises
let, if, and iteration premises, which the property evaluator now supports:

  $ spectec impty quickcheck --spec ../../testdata/quickcheck/preservation-compat.spectec --num-tests 50 --color never
  [Quickcheck Type_safety: Test]
  OK, passed 50 samples.
  [Quickcheck Preservation: Test]
  OK, passed 50 samples.
