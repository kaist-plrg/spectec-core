struct d {
  bit a;
  bit b;
} control c() {
  apply { bool e = {a = 1, b = 2} == (d){a = 1, b = 2}; }
}
