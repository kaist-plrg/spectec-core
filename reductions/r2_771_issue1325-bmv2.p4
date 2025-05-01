error { a }
struct b {
  error c;
} struct d {
  b e;
} control f(d g) {
  apply {
    if (g.e.c == error.a)
      ;
  }
}
