bit<16> a(in bit<16> b) {
  bit<16> h;
  return h;
}
header c { bit<48> d; }
struct e {
  c ethernet;
} control f(inout e g) {
  apply { g.ethernet.d [15:0] = a(g.ethernet.d [15:0]); }
}
