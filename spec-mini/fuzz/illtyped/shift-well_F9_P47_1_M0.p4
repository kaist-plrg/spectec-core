// Intended pid 47
// Source vid 25
// Depth 2

// Mutation GenFromTyp

/*
From (NameT "Hdr")
To (FBitT (NumE (INT +2)))
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in bit<32> x, in bit<2> hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 47 }
