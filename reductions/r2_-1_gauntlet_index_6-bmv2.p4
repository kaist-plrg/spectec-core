header b { bit<8> a; }
struct c {
  b[3] d;
} control e(c d) {
  action f(bit<8> g) {}
  apply { f(d.d[1].a); }
}
