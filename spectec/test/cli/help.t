impty CLI: help text, locking the subcommand and flag surface.

The impty group lists its subcommands:

  $ spectec impty --help
  impty commands
  
    spectec impty SUBCOMMAND
  
  === subcommands ===
  
    batch                      . Run batch over all impty input specs
    checkpoint                 . Checkpoint utilities
    eval                       . Run impty evaluator
    parse                      . parse an impty program to an IL value
    typecheck                  . Run impty typechecker
    help                       . explain a given subcommand (perhaps recursively)
  

The typecheck subcommand carries the full flag surface, including every
instrumentation handler (one --<handler>.level / --<handler>.output pair each);
eval shares this exact set:

  $ spectec impty typecheck --help
  Run impty typechecker
  
    spectec impty typecheck 
  
  === flags ===
  
    -p FILE                    . impty file
    [--batch-dir DIR]          . directory of inputs (default: target's test dir)
    [--batch]                  . run on a directory of inputs
    [--branch-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--branch-coverage.output FILE]
                               . output destination file
    [--color WHEN]             . colorize diagnostics: auto|always|never (default:
                                 auto)
    [--instruction-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--instruction-coverage.output FILE]
                               . output destination file
    [--pl]                     . use PL interpreter (default: IL)
    [--premise-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--premise-coverage.output FILE]
                               . output destination file
    [--profile.output FILE]    . output destination file
    [--sl]                     . use SL interpreter (default: IL)
    [--spec FILES] ...         . spec files; mutually exclusive with --spec-dir
    [--spec-dir DIR]           . directory of .spectec files, collected
                                 recursively; mutually exclusive with --spec
    [--trace.level LEVEL]      . verbosity level: summary|rules|inputs|full
    [--trace.output FILE]      . output destination file
    [--tree.level LEVEL]       . verbosity level: rule|conclusion|premise
    [--tree.output FILE]       . output destination file
    [-v]                       . verbose output
    [-help], -?                . print this help text and exit
  

parse has a distinct, minimal surface with no instrumentation flags:

  $ spectec impty parse --help
  parse an impty program to an IL value
  
    spectec impty parse 
  
  === flags ===
  
    -p FILE                    . impty file
    [--color WHEN]             . colorize diagnostics: auto|always|never (default:
                                 auto)
    [--spec FILES] ...         . spec files; mutually exclusive with --spec-dir
    [--spec-dir DIR]           . directory of .spectec files, collected
                                 recursively; mutually exclusive with --spec
    [-r]                       . roundtrip parse/unparse
    [-help], -?                . print this help text and exit
  

batch drops -p and adds the checkpoint-persistence flags:

  $ spectec impty batch --help
  Run batch over all impty input specs
  
    spectec impty batch 
  
  === flags ===
  
    [--batch-dir DIR]          . directory of inputs (default: target's test dir)
    [--branch-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--branch-coverage.output FILE]
                               . output destination file
    [--checkpoint FILE]        . save checkpoint to file (enables resume)
    [--color WHEN]             . colorize diagnostics: auto|always|never (default:
                                 auto)
    [--instruction-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--instruction-coverage.output FILE]
                               . output destination file
    [--pl]                     . use PL interpreter (default: IL)
    [--premise-coverage.level LEVEL]
                               . verbosity level (e.g., summary|full)
    [--premise-coverage.output FILE]
                               . output destination file
    [--profile.output FILE]    . output destination file
    [--resume FILE]            . resume from checkpoint file
    [--save-interval N]        . save checkpoint every N tests (default: 100)
    [--sl]                     . use SL interpreter (default: IL)
    [--spec FILES] ...         . spec files; mutually exclusive with --spec-dir
    [--spec-dir DIR]           . directory of .spectec files, collected
                                 recursively; mutually exclusive with --spec
    [--trace.level LEVEL]      . verbosity level: summary|rules|inputs|full
    [--trace.output FILE]      . output destination file
    [--tree.level LEVEL]       . verbosity level: rule|conclusion|premise
    [--tree.output FILE]       . output destination file
    [-v]                       . verbose: print progress for each test
    [-help], -?                . print this help text and exit
  
