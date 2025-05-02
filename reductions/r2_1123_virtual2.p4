extern a {
  a();
  abstract bit<16> b(in bit<16> d);
}
extern e { e(int<16> f); }
control c() {
  a() cntr = { e(16s1024) g;
  bit<16> b(in bit<16> d) { return d; }
};
apply {}
}
