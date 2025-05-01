extern a { void c(); }
extern d {}
struct e {
} parser f<g, i>(a b, out g j, inout i k, inout e l);
control n<g, i>(inout g hdr, inout i k);
control o<g, i>(inout g hdr, inout i k, inout e l);
control w<g, i>(inout g hdr, inout i k, inout e l);
control x<g, i>(inout g hdr, inout i k);
control q<g>(d b, in g hdr);
package r<g, i>(f<g, i> p, n<g, i> s, o<g, i> ig, w<g, i> eg, x<g, i> ck,
                q<g> dep);
struct t {
} struct u {
} parser p(a pkt, out t hdr, inout u m, inout e sm) {
  state start {}
}
control ingress(inout t h, inout u m, inout e sm) {
  apply {}
}
control v(inout t h, inout u m) {
  apply {}
}
control update(inout t h, inout u m) {
  apply {}
}
control egress(inout t h, inout u m, inout e sm) {
  apply {}
}
control deparser(d pkt, in t h) {
  apply {}
}
r(p(), v(), ingress(), egress(), update(), deparser()) main;
