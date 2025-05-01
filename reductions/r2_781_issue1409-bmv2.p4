header a {}
struct b {
  a[2] c;
} parser d(b e) {
  state start { bit<32> f = e.c.lastIndex; }
}
