header a { bit b; }
struct d {
  a[1] c;
} control e(inout d f) {
  action g() { f.c[0].b = f.maxSizeInBytes(); }
  apply {}
}
