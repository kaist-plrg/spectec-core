extern a { void c<d>(d e); }
struct f {
} control deparser(a b, f g) {
  apply { b.c(g); }
}
