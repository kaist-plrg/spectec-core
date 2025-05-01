header a { bit b; }
struct f {
  a[5] c;
} parser d(f e) {
  state start {
    transition select(e.c.last.b) {}
  }
}
