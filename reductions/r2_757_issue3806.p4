header a {
  bit b;
  bit h;
  bit c;
}
struct d {
  a e;
} void f(inout d g) {
  g.e = g.e.c == 1 ? {1, 1, 1} : {2, 2, 2};
}
