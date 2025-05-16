// Intended pid 43
// Source vid 5
// Depth 4

// Mutation MixopGroup

/*
From (FBitT (NumE (INT +32)))
To (FIntT (NumE (INT +32)))
*/

header Hdr {
  int<32> src;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 43 }
