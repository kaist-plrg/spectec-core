extern a { void b<c>(out c d); }
header e {}
struct f {
  e next;
} parser g(a h, out f headers) {
  state start { h.b(headers.next); }
}
