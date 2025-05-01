header a { bit<32> b; }
parser c(out a d) {
  state start { d.b [3:0] = 2; }
}
