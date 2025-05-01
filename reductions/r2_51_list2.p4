extern a<b, d> { a(list<tuple<b, d>> data); }
control c() {
  a((list<tuple<bit, bit>>){{2, 3}}) e;
  apply {}
}
