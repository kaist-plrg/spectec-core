Installing the Impty target package adds its command to the core executable.

  $ grep -q 'target_plugins/impty/META' ../../../../spectec-target-impty.install

  $ spectec --help | grep '^  impty'
    impty                      . impty commands

  $ spectec impty parse -p ../../../testdata/interp/impty/base/hello.imp --color never
  int x = 5;
  bool y = x <= 10
