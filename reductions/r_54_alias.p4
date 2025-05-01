header b {}
struct d {
  b[3] f;
  bit<32> a;
} extern g {
  g();
  void h(out bit<32> i, out d j);
}
control c() {
  g() e;
  apply {
    d k;
    e.h(k.a, k);
  }
}
