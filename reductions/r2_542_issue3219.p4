bit<4> a() {
  bit<4> b;
  return b;
}
control c() {
  apply { a(); }
}
