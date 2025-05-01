header a {}
struct b {
  a[2] c;
} parser d(b e) {
  state start {
    transition select(e.c.lastIndex) {}
  }
}
