extern g {}
extern h {}
struct i {
} struct j {
} struct k {
} parser l<m, n, o, p>(g a, out m b, inout n q, in i c, in o r, in p s);
control t<m, n>(inout m d, inout n q, in j c, inout k e);
control u<m, n, v, o, w>(h a, out v x, out o r, out w y, inout m d, in n f,
                         in k c);
package z<ab, ac, w, v, o, p>(l<ab, ac, o, p> e, t<ab, ac> f,
                              u<ab, ac, v, o, w> ae);
struct af {
} struct ag {
} struct ah {
} parser ai(g a, out ah ad, inout ag b, in i c, in af d, in af e) {
  state start {}
}
control aj(inout ah ad, inout ag b, in j c, inout k d) {
  apply {}
}
control ak(h aa, out af a, out af b, out af c, inout ah d, in ag e, in k f) {
  apply {}
}
z(ai(), aj(), ak()) e;
