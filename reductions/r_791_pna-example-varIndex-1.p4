extern a { void b<c>(in c d); }
header e {}
struct f {
  e[2] g;
} control h(a i, f d) {
  apply { i.b(d.g[0]); }
}
