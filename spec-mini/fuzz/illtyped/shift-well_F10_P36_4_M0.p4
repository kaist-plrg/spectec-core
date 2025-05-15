// Intended pid 36
// Source vid 32
// Depth 3

// Mutation GenFromTyp

/*
From (NameE "hdr")
To (NumE (INT +0))
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) << (0.src));
}

// Covered pids { 36 }
