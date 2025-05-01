struct a {
  bit b;
  bit g;
} struct c {
  a d;
} control e(inout c f) {
  apply { f.d = {b = 2, g = 3}; }
}
