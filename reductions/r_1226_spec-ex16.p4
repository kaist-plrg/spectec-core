extern a {}
parser c<e>(a b, out e f);
control g<e>(in e d);
package h<e>(c<e> i, g<e> m);
parser j(a b, out bit<32> d) {
  state start {}
}
control k(in bit<32> d) {
  apply {}
}
h(j(), k()) l;
