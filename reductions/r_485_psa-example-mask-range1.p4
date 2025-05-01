extern a {
  void b<c>(out c d);
  void b<c>(c e, bit f);
}
struct g {
} parser h(a buffer, out g d) {
  state start { buffer.b(d); }
}
