impty parse -r: the surface printer round-trips every construct.

A program exercising arrow types (nested), function literals and calls, the
infix and unary operators, negative literals, and every command form re-parses
to the same tree (the -r flag asserts this; on failure it prints "Roundtrip
failed" and exits nonzero instead of the surface below):

  $ SPEC=../../specs/impty/closure

  $ cat > rt.imp <<EOF
  > (int -> int) -> int f = fun ((int -> int) g) -> int { g(1) + 2 };
  > int x = 1 + (2 + 3);
  > int z = -5 + 1;
  > bool y = !(1 <= 2) && (3 <= 4);
  > if y then x = f(fun (int n) -> int { n }) else skip end;
  > while x <= 10 do x = x + 1 end
  > EOF

  $ spectec impty parse -r --spec-dir $SPEC -p rt.imp --color never
  (int -> int) -> int f = fun (int -> int g) -> int { g(1) + 2 };
  int x = 1 + (2 + 3);
  int z = -5 + 1;
  bool y = !(1 <= 2) && 3 <= 4;
  if y then x = f(fun (int n) -> int { n }) else skip end;
  while x <= 10 do x = x + 1 end
