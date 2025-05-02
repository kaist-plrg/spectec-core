header a { bit b; }
struct f {
  a c;
} control d(inout f e) {
  apply { e.c.b = 0 / 8; }
}
