header Hdr { bit<32> src; }
bit<32> f(in bit<32> x, in Hdr hdr) {
  return x << hdr.src;
}
