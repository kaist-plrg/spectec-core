extern a { void b<c>(c d); }
struct e {
} control deparser(a f, e d) {
  apply { f.b(d); }
}
