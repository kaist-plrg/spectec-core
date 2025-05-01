header a { bit<32> b; }
header c {}
bit<32> d(a e) {
  c f;
  const bit g = f.minSizeInBits();
  return e.b;
}
