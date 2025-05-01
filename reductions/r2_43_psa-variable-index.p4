extern a { void b<c>(out c d); }
enum bit e { f = 0 } header g { e l; }
struct i {
  g j;
} parser k(a buffer, out i h) {
  state start { buffer.b(h); }
}
