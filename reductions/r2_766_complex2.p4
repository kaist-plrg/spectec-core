extern bit a(in bit b);
header d {}
control c() {
  apply {
    d[2] e;
    e[a(2)].setValid();
  }
}
