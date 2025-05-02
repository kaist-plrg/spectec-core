extern a {}
header b {}
parser c(a d, out b e) {
  state start {}
}
parser f(a d, out b[2] e) {
  c() g;
  state start { g.apply(d, e[0]); }
}
