header a {}
struct b {
  a c;
  a d;
} control e() {
  b f;
  b g;
  apply { g = {f.c, f.d}; }
}
