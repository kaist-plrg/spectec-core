extern a { a(); }
parser b() {
  a() a;
  state start {}
}
