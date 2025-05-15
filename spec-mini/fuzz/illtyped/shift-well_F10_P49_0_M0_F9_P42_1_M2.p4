// Intended pid 42
// Source vid 19
// Depth 2

// Mutation MixopGroup

/*
From ("x" (IN) (FBitT (NumE (INT +32))) None)
To ("x" (IN) (FIntT (NumE (INT +32))) None)
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in int<32> x, in Hdr hdr) {
  return ((x) - (hdr.src));
}

// Covered pids { 42 }
