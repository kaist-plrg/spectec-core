struct d {
  bit<32> e;
} control c(inout bit<32> b) {
  action a() {
    d f;
    b = f.e;
  }
  apply {}
}
