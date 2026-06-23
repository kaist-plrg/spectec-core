impty parse -r re-parses the printed surface and asserts the same tree, over a
program covering every construct:

  $ SPEC=../../specs/impty/recursion/spec.spectec

  $ cat > rt.imp <<EOF
  > int a = -5 + 1;
  > bool b = !(1 <= 2) && 3 <= 4;
  > (int -> int) -> int f = fun ((int -> int) g) -> int { g(1) + 2 };
  > int c = (a <= 0) ? 0 : a + 1;
  > rec sum (int i) -> int { 10 <= i ? 0 : i + sum(i + 1) };
  > if b then a = f(fun (int n) -> int { n }) else skip end;
  > while a <= 10 do a = a + 1 end
  > EOF

  $ spectec impty parse -r --spec $SPEC -p rt.imp --color never
  int a = -5 + 1;
  bool b = !(1 <= 2) && 3 <= 4;
  (int -> int) -> int f = fun (int -> int g) -> int { g(1) + 2 };
  int c = a <= 0 ? 0 : a + 1;
  rec sum (int i) -> int { 10 <= i ? 0 : i + sum(i + 1) };
  if b then a = f(fun (int n) -> int { n }) else skip end;
  while a <= 10 do a = a + 1 end
