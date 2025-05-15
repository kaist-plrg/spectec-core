// Intended pid 39
// Source vid 2
// Depth 1

// Mutation GenFromTyp

/*
From +32
To -2
*/

header Hdr {
  bit<-2> src;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 39 }
