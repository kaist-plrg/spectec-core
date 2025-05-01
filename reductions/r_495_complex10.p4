extern bit a(in bit b);
control c() {
  apply {
    if (a(5) == 2)
      ;
  }
}
