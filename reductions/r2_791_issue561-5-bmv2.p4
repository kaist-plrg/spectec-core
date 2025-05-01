extern a { void b<c>(out c d); }
header e {}
header_union f { e short; }
struct g {
  f[1] h;
} parser i(a j, out g d) {
  state start { j.b(d.h[0].short); }
}
