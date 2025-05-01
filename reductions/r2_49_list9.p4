struct a<b> {
  b d;
} extern f {
  f(list<a<bit<32>>> data);
}
control c() {
  f((list<a<bit<32>>>){{0}}) e;
  apply {}
}
