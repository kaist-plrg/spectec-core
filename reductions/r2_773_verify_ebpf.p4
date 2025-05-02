extern void a(in bool b, error c);
header d { bit e; }
struct i {
  d f;
} error{g} parser h(i headers) {
  state start { a(headers.f.e == 1, error.g); }
}
