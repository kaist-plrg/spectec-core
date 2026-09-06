impty CLI: project-local defaults from spectecx.config.

A target reads its own namespaced keys, so impty.spec_dir lets typecheck run
without --spec:

  $ cat > spectecx.config <<EOF
  > impty.spec_dir = ../../specs/impty/base
  > EOF
  $ spectec impty typecheck -p ../../testdata/interp/impty/base/hello.imp --color never
  Typecheck succeeded

A key for another target is ignored. With only a p4 key, Impty uses its packaged default specification:

  $ cat > spectecx.config <<EOF
  > p4.spec_dir = ../../specs/impty/base
  > EOF
  $ spectec impty typecheck -p ../../testdata/interp/impty/base/hello.imp --color never
  Typecheck succeeded


An explicit --spec overrides the config:

  $ cat > spectecx.config <<EOF
  > impty.spec_dir = /nonexistent
  > EOF
  $ spectec impty typecheck --spec ../../specs/impty/base/spec.spectec -p ../../testdata/interp/impty/base/hello.imp --color never
  Typecheck succeeded

Setting both spec and spec_dir for a target is a config error:

  $ cat > spectecx.config <<EOF
  > impty.spec = ../../specs/impty/base/spec.spectec
  > impty.spec_dir = ../../specs/impty/base
  > EOF
  $ spectec impty typecheck -p ../../testdata/interp/impty/base/hello.imp --color never
  error: spectecx.config sets both 'impty.spec' and 'impty.spec_dir'; use one or the other
  
    source: config
  [1]


batch reads spec_dir and batch_dir from the config when the flags are omitted:

  $ cat > spectecx.config <<EOF
  > impty.spec_dir = ../../specs/impty/base
  > impty.batch_dir = ../../testdata/interp/impty/base
  > EOF
  $ spectec impty batch --color never
  typechecker: 14/14 passed, 0 failed
  evaluator: 14/14 passed, 0 failed
