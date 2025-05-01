struct a<c> {
  c d;
  bit e;
} struct g {
  bit b;
} struct h {
  a<g> f;
} const h i = {{d = {0}, e = 1}};
