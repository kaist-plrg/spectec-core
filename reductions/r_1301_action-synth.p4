extern a {}
extern c {}
struct d {
} parser e<f, g>(a b, out f h, inout g i, inout d t);
control j<f, g>(inout f hdr, inout g i);
control k<f, g>(inout f hdr, inout g i, inout d t);
control l<f, g>(inout f hdr, inout g i, inout d t);
control m<f, g>(inout f hdr, inout g i);
control n<f>(c b, in f hdr);
package o<f, g>(e<f, g> p, j<f, g> u, k<f, g> ig, l<f, g> eg, m<f, g> ck,
                n<f> dep);
struct f {
} struct g {
} parser v(a pk, out f hdr, inout g i, inout d smeta) {
  state start {}
}
control q(inout f hdr, inout g i, inout d smeta) {
  apply {}
}
control r(inout f hdr, inout g i, inout d smeta) {
  apply {}
}
control s(c pk, in f hdr) {
  apply {}
}
control w(inout f hdr, inout g i) {
  apply {}
}
control x(inout f hdr, inout g i) {
  apply {}
}
o(v(), w(), q(), r(), x(), s()) main;
