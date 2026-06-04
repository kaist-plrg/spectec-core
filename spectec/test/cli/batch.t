impty CLI: batch run over the base corpus.

  $ SPEC=../../specs/impty/base/spec.spectec

The typechecker and evaluator each run over every .imp under the base dir; the
summary counts the expected-fail _errors_ cases as passing:

  $ spectec impty batch --spec $SPEC --batch-dir ../../testdata/interp/impty/base --color never
  typechecker: 14/14 passed, 0 failed
  evaluator: 14/14 passed, 0 failed

When the batch dir holds no matching inputs, the run errors instead of
reporting 0/0 passed:

  $ mkdir empty
  $ spectec impty batch --spec $SPEC --batch-dir empty --color never
  error: batch run collected no inputs under empty; pass --batch-dir or set the target's batch_dir in spectecx.config
  
    source: config
  [1]


Running the base spec over both variants leaves the closure tests failing;
each is listed by name under the tally:

  $ spectec impty batch --spec $SPEC --batch-dir ../../testdata/interp/impty --color never
  typechecker: 17/23 passed, 6 failed
    fail             ../../testdata/interp/impty/closure/closure.imp
    fail             ../../testdata/interp/impty/closure/curried.imp
    fail             ../../testdata/interp/impty/closure/higher_order.imp
    fail             ../../testdata/interp/impty/closure/static-vs-dynamic.imp
    fail             ../../testdata/interp/impty/recursion/rec_countup.imp
    fail             ../../testdata/interp/impty/recursion/rec_sum.imp
  evaluator: 17/23 passed, 6 failed
    fail             ../../testdata/interp/impty/closure/closure.imp
    fail             ../../testdata/interp/impty/closure/curried.imp
    fail             ../../testdata/interp/impty/closure/higher_order.imp
    fail             ../../testdata/interp/impty/closure/static-vs-dynamic.imp
    fail             ../../testdata/interp/impty/recursion/rec_countup.imp
    fail             ../../testdata/interp/impty/recursion/rec_sum.imp

A valid program named like an expected-fail case is reported as an unexpected
pass:

  $ mkdir corpus
  $ cp ../../testdata/interp/impty/base/hello.imp corpus/_errors_valid.imp
  $ spectec impty batch --spec $SPEC --batch-dir corpus --color never
  typechecker: 0/1 passed, 1 failed
    unexpected pass  corpus/_errors_valid.imp
  evaluator: 0/1 passed, 1 failed
    unexpected pass  corpus/_errors_valid.imp

