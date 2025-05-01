header b { bit c; }
header d { bit<32> a; }
struct e {
  b f;
  d[2] g;
} control h(e g) {
  action i(bit<32> dummy) {}
  apply { i(g.g[g.f.c].a); }
}
