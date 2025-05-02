struct a {
  bit b;
} header c {
  a d;
}
struct e {
  c f;
} control g(inout e h) {
  apply { h.f.d = {b = 6}; }
}
