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
struct headers {
} struct metadata {
} parser v(a packet, out headers hdr, inout metadata i, inout d t) {
  state start {}
}
control q(inout headers hdr, inout metadata i, inout d t) {
  apply {}
}
control r(inout headers hdr, inout metadata i) {
  apply {}
}
control s(inout headers hdr, inout metadata i, inout d t) {
  apply {}
}
control w(inout headers hdr, inout metadata i) {
  apply {}
}
control x(c packet, in headers hdr) {
  apply {}
}
o(v(), r(), q(), s(), w(), x()) main;
