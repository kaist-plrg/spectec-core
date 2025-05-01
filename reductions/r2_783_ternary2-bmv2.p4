header a { bit b; }
struct f {
  a[4] c;
} parser d(f e) {
  state start {
    transition select(e.c.last.b) {}
  }
}
