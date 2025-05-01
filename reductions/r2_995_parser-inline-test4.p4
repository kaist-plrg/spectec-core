extern a {}
struct b {
} struct c {
} parser d(a e, out b f, inout c g) {
  state start {}
}
parser h(a e, out b f) {
  d() i;
  c j;
  state start { i.apply(e, f, j); }
}
