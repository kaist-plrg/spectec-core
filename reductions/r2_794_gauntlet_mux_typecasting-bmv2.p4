extern a { void b<c>(out c d); }
header e {}
struct f {
  e g;
} parser h(a i, out f d) {
  state start { i.b(d.g); }
}
