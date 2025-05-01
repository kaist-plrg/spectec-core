header b { bit<48> c; }
header d { bit<8> a; }
struct e {
  d f;
} control g(inout e f) {
  b h;
  apply { f.f.a = h.c [16:9]; }
}
