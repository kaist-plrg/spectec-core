struct a<c> {
  c d;
  bit e;
} struct f {
  bit b;
} struct h<i> {
  a<i> g;
  bit invalid;
} const h<f> j = {g = (a<f>){d = {0}, e = 1}, invalid = 1};
