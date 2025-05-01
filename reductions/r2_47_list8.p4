extern a { a(list<list<bit<32>>> b); }
control c() {
  a((list<list<bit<32>>>){{3}}) d;
  apply {}
}
