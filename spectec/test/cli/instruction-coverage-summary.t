impty CLI: instruction-coverage summary against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Instruction coverage is SL-only; run eval under the SL interpreter. Summary
lists only the uncovered SL instructions:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --sl --instruction-coverage.level summary
  
  === SL Node Coverage ===
  
  SL Instructions: 65/154 (42.21%)
  
  Uncovered SL instructions:
    lookup:
      If ((pair<K, V>* matches pattern _ :: _))
    Check_value:
      Case on literal
  [
    y -> true,
    x -> 5
  ]
