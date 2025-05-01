enum bit a { b = 0 } header c { a d; }
struct e {
  c[2] f;
} parser g(e h) {
  state start {
    transition select(h.f.last.d) {}
  }
}
