header a { int<8> b; }
struct c {
  a d;
} enum int<8> e{f = 2} control g(c h) {
  apply { e i = (e)h.d.b; }
}
