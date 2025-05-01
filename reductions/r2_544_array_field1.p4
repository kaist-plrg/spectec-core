extern bit a(inout bit c, bit b);
control d() {
  bit e;
  action act() { a(e, 1w0); }
  apply {}
}
