extern a { void b<c>(out c d, in bit<32> e); }
header f { bit<32> size; }
struct g {
  f h;
} parser i(a j, out g hdr) {
  state start { j.b(hdr, hdr.h.size); }
}
