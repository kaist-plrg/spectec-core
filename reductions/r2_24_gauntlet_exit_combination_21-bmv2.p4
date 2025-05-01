extern a { void c<d>(d e); }
extern f {}
struct g {
} parser i<j, k>(a b, out j l, inout k meta, inout g n);
control o<j, k>(inout j e, inout k meta);
control w<j, k>(inout j e, inout k meta, inout g n);
control x<j, k>(inout j e, inout k meta, inout g n);
control q<j, k>(inout j e, inout k meta);
control r<j>(f b, in j e);
package s<j, k>(i<j, k> p, o<j, k> t, w<j, k> ig, x<j, k> eg, q<j, k> ck,
                r<j> dep);
struct u {
} struct v {
} parser p(a pkt, out u e, inout v m, inout g sm) {
  state start {}
}
control ingress(inout u h, inout v m, inout g sm) {
  apply {}
}
control vrfy(inout u h, inout v m) {
  apply {}
}
control update(inout u h, inout v m) {
  apply {}
}
control egress(inout u h, inout v m, inout g sm) {
  apply {}
}
control deparser(f pkt, in u h) {
  apply {}
}
s(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
