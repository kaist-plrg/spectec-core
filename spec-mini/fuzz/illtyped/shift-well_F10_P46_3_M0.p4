// Intended pid 46
// Source vid 6
// Depth 4

// Mutation GenFromTyp

/*
From ("src", (FBitT (NumE (INT +32))))
To ("b", (FBitT (NumE (INT +32))))
*/

header Hdr {
  bit<32> b;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 46 }
