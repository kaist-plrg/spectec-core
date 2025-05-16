// Intended pid 49
// Source vid 36
// Depth 2

// Mutation MixopGroup

/*
From (BinE (SHL) (NameE "x") (ExprAccE (NameE "hdr") "src"))
To (BinE (MINUS) (NameE "x") (ExprAccE (NameE "hdr") "src"))
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) - (hdr.src));
}

// Close-missed pids { 42 }
