Installing the P4 target package adds its command and default specifications.

  $ grep -q 'target_plugins/p4/META' ../../../../spectec-target-p4.install

  $ spectec --help | grep '^  p4'
    p4                         . P4 commands

  $ spectec p4 parse -p ../../../testdata/interp/p4/p4c/p4_16_samples/issue2342.p4 --color never 2>/dev/null | head -n 1
  const bool tmp = d 1 != 8 w 2 [d 7 : d 0] ;
