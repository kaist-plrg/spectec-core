Installing the Mini-ML target package adds its command and default specification.

  $ grep -q 'target_plugins/miniml/META' ../../../../spectec-target-miniml.install

  $ spectec --help | grep '^  miniml'
    miniml                     . Mini-ML commands

  $ spectec miniml parse -p ../../../testdata/interp/miniml/add.ml --color never
  + (40, 2)
