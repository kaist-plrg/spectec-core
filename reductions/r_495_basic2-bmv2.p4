enum a {
  b
} extern void
c<d, e>(in bool condition, in d data, inout e checksum, a algo);
header f {}
struct g {
  f h;
} control i(inout g hdr) {
  apply { c(hdr.h.isValid(), {hdr}, hdr, a.b); }
}
