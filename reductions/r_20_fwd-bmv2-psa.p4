extern a { void b<c>(out c d); }
struct e {
} parser f(a buffer, out e g) {
  state start { buffer.b(g); }
}
