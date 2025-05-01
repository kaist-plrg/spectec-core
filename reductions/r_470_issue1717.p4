header a { bit<32> b; }
bit<32> c(a d) {
  const bit e = d.minSizeInBytes();
  return d.b;
}
