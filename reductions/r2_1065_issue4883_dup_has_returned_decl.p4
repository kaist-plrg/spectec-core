struct a {
} control b(inout a c) {
  apply {}
}
control d() {
  apply {
    a c;
    b.apply(c);
  }
}
