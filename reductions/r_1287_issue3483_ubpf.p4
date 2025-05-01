extern a { void c(); }
extern d {}
struct e {
} parser f<g, h>(a i, out g headers, inout h j, inout e k);
control l<g, h>(inout g headers, inout h j, inout e k);
control deparser<g>(d b, in g headers);
package m<g, h>(f<g, h> n, l<g, h> o, deparser<g> dprs);
struct r {
} struct p {
} parser n(a o, out r headers, inout p j, inout e s) {
  state start {}
}
control pipe(inout r headers, inout p j, inout e q) {
  apply {}
}
control dprs(d i, in r headers) {
  apply {}
}
m(n(), pipe(), dprs()) main;
