// Intended pid 28
// Source vid 28
// Depth 4

// Mutation MutateList

/*
From [ ("x" (IN) (FBitT (NumE (INT +32))) None),
  ("hdr" (IN) (NameT "Hdr") None) ]
To [ ("x" (IN) (FBitT (NumE (INT +32))) None),
  ("x" (IN) (FBitT (NumE (INT +32))) None),
  ("hdr" (IN) (NameT "Hdr") None) ]
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in bit<32> x, in bit<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 28 }
