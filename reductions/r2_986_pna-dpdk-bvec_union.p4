header_union a {}
struct b {
  a c;
} control d(b e) {
  apply {
    if (e.c.isValid())
      ;
  }
}
