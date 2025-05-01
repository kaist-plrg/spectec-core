enum bit<16> a{b = 0} header c { bit<16> d; }
struct e {
  c ethernet;
} parser f(e g) {
  state start {
    transition select(g.ethernet.d) {
    b:
      h;
    }
  }
  state h {}
}
