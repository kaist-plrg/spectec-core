extern a {
  a();
  void add<b>(in b c);
}
control d() {
  a() ck;
  apply { ck.add({}); }
}
