Mini-ML CLI: surface behavior.

  $ SPEC=../../specs/miniml/spec.spectec
  $ CORPUS=../../testdata/interp/miniml

The command group presents the language name and surface operations consistently:

  $ spectec miniml --help
  Mini-ML commands
  
    spectec miniml SUBCOMMAND
  
  === subcommands ===
  
    checkpoint                 . Checkpoint utilities
    eval                       . Run Mini-ML evaluator
    parse                      . Parse a Mini-ML program to an IL value
    help                       . explain a given subcommand (perhaps recursively)
  

The evaluator prints a Mini-ML surface value:

  $ spectec miniml eval --spec $SPEC -p $CORPUS/fix1.ml --color never
  55

Applications with a function-valued left operand round-trip without losing required parentheses:

  $ spectec miniml parse --spec $SPEC -p $CORPUS/beta1.ml --color never -r
  (fun x -> x) 42
  $ spectec miniml parse --spec $SPEC -p $CORPUS/beta2.ml --color never -r
  (fun x -> + (x, x)) 21
