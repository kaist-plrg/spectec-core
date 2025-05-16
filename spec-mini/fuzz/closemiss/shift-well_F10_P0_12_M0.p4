// Intended pid 0
// Source vid 18
// Depth 4

// Mutation MixopGroup

/*
From (FBitT (NumE (INT +32)))
To (FIntT (NumE (INT +32)))
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in int<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Close-missed pids { 38, 51 }
