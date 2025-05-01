extern a { void b<c>(out c d); }
struct e {
} parser f(a buffer, out e d) {
  state start { buffer.b(d); }
}
