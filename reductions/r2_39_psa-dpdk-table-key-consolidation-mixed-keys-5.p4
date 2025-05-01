extern a {}
extern b {}
enum c { d } struct e { c f; } struct g {
} struct h {
} parser i<j, k, l, m>(a aa, out j n, inout k o, in e a, in l b, in m p);
control ab<j, k>(inout j d, inout k o, in g a, inout h e);
control q<j, k, r, l, s>(b aa, out r t, out l b, out s u, inout j d, in k f,
                         in h a);
package v<w, x, s, r, l, m>(i<w, x, l, m> c, ab<w, x> d, q<w, x, r, l, s> e);
struct y {
} struct ac {
} struct ad {
} parser z(a aa, out ad d, inout ac o, in e a, in y b, in y p) {
  state start {}
}
control ae(inout ad d, inout ac o, in g a, inout h e) {
  apply {}
}
control af(b ag, out y t, out y b, out y u, inout ad d, in ac f, in h a) {
  apply {}
}
v(z(), ae(), af()) c;
