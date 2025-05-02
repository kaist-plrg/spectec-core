extern g {}
extern i {}
struct j {
} struct k {
} struct l {
} parser m<n, o, p, aa>(g b, out n q, inout o r, in j c, in p s, in aa t);
control u<n, o>(inout n d, inout o r, in k c, inout l e);
control v<n, o, w, p, x>(i b, out w y, out p s, out x z, inout n d, in o f,
                         in l c);
package ac<ad, ae, x, w, p, aa>(m<ad, ae, p, aa> f, u<ad, ae> af,
                                v<ad, ae, w, p, x> a);
struct ag {
} struct ah {
} struct ai {
} struct aj {
} struct ak {
} struct al {
} parser am(g ab, out al h, inout ag b, in j c, in ah d, in ak e) {
  state start {}
}
control an(inout al a, inout ag b, in k c, inout l d) {
  apply {}
}
control ao(i ab, out ai a, out ah b, out aj c, inout al d, in ag e, in l f) {
  apply {}
}
ac(am(), an(), ao()) f;
