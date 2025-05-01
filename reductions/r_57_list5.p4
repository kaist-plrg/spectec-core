typedef bit a;
extern b<f, d> { b(list<tuple<a, tuple<f, f>, d>> data); }
control c() {
  b((list<tuple<a, tuple<bit<32>, bit<32>>, bit>>){{0, {2, 5}, 3}}) e;
  apply {}
}
