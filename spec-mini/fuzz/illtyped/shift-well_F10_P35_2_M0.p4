// Intended pid 35
// Source vid 19
// Depth 3

// Mutation GenFromTyp

/*
From ("x" (IN) (FBitT (NumE (INT +32))) None)
To ("b" (IN) (FBitT (NumE (INT +32))) None)
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in bit<32> b, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 35 }
