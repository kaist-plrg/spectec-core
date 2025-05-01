enum a { b } struct c {
  bit<32> d;
} control e(inout c f) {
  a g;
  action h() { f.d = g == a.b ? 2 : 32w0; }
  apply {}
}
