// Intended pid 38
// Source vid 16
// Depth 1

// Mutation GenFromTyp

/*
From +32
To -2
*/

header Hdr {
  bit<32> src;

}
bit<32> f(in int<-2> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 38 }
