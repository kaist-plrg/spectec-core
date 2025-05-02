struct b {
  bit d;
} extern void e(in tuple<bit> a, in b f);
control c() {
  apply { e({0}, {0}); }
}
