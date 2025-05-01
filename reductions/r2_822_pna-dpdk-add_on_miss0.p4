extern a { void b<c>(out c d); }
header e {}
struct f {
  e ethernet;
} parser g(a h, out f d) {
  state start { h.b(d.ethernet); }
}
