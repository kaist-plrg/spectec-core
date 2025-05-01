extern a { void b<c>(out c d); }
header e {}
parser f(a g, out e[2] h) {
  bit i;
  state start { g.b(h[i]); }
}
