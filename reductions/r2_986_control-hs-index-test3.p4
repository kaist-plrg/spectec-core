header a { bit b; }
header g {}
struct c {
  g[3] d;
  a e;
} control f(c d) {
  apply {
    if (d.d[d.e.b].isValid())
      ;
  }
}
