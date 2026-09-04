set -eu

run_spectec() {
  opam exec -- spectec "$@"
}

assert_missing() {
  command=$1
  if run_spectec --help | grep -Eq "^  ${command}[[:space:]]"; then
    printf 'unexpected target command: %s\n' "$command" >&2
    exit 1
  fi
}

check_target() {
  package=$1
  command=$2
  shift 2

  opam install --yes --with-test "./${package}.opam"
  run_spectec --help | grep -Eq "^  ${command}[[:space:]]"
  run_spectec "$@"
  opam remove --yes "$package"
  assert_missing "$command"
}

opam install --yes --with-test ./spectec.opam
assert_missing impty
assert_missing miniml
assert_missing p4

check_target spectec-target-p4 p4 p4 parse -p spectec/testdata/interp/p4/p4c/p4_16_samples/issue2342.p4 --color never
check_target spectec-target-miniml miniml miniml parse -p spectec/testdata/interp/miniml/add.ml --color never
check_target spectec-target-impty impty impty parse -p spectec/testdata/interp/impty/base/hello.imp --color never
