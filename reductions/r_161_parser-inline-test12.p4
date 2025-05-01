extern a { void b(); }
struct c {
} struct d {
} parser e(a f, out c g, inout d h) {
  state start {}
}
parser i(a f, out c g) {
  e() k;
  d j;
  state start { k.apply(f, g, j); }
}
