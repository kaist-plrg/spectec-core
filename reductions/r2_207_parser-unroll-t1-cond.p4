header a { bit b; }
struct f {
  a c;
} parser d(f e) {
  state start {
    transition select(e.c.b) {}
  }
}
