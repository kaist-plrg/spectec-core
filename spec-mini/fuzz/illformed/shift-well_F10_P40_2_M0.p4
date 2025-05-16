// Intended pid 40
// Source vid 8
// Depth 4

// Mutation GenFromTyp

/*
From (HeaderD "Hdr" [ ("src", (FBitT (NumE (INT +32)))) ])
To (HeaderD "a" [ ("src", (FBitT (NumE (INT +32)))) ])
*/

header a {
  bit<32> src;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 40 }
