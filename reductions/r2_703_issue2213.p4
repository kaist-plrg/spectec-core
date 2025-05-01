struct a {
  bit b;
} struct h {
  a c;
} struct d {
  h e;
} control f(inout d g) {
  apply { g.e = {{b = 2}}; }
}
