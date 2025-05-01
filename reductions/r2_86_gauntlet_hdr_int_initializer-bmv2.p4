extern a {}
extern c {}
struct d {
} parser e<f, g>(a b, out f i, inout g j, inout d k);
control l<f, g>(inout f hdr, inout g j);
control n<f, g>(inout f hdr, inout g j, inout d k);
control o<f, g>(inout f hdr, inout g j, inout d k);
control v<f, g>(inout f hdr, inout g j);
control w<f>(c b, in f hdr);
package q<f, g>(e<f, g> p, l<f, g> r, n<f, g> ig, o<f, g> eg, v<f, g> ck,
                w<f> dep);
struct s {
} struct t {
} parser p(a pkt, out s hdr, inout t m, inout d sm) {
  state start {}
}
control ingress(inout s h, inout t m, inout d sm) {
  apply {}
}
control u(inout s h, inout t m) {
  apply {}
}
control update(inout s h, inout t m) {
  apply {}
}
control egress(inout s h, inout t m, inout d sm) {
  apply {}
}
control deparser(c pkt, in s h) {
  apply {}
}
q(p(), u(), ingress(), egress(), update(), deparser()) main;
