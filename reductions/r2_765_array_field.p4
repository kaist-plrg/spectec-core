header c { bit d; }
extern bit e(inout bit f, in bit b);
control g(out c[2] h) {
  apply {
    bit a;
    e(h[a].d, 1);
  }
}
