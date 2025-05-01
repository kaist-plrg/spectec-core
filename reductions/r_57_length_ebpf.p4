extern a {}
parser b<c>(a d, out c e);
control f<c>(inout c e, out bool accept);
package g<c>(b<c> h, f<c> filt);
struct i {
} parser h(a m, out i e) {
  state start {}
}
control j(inout i e, out bool k) {
  apply {}
}
g(h(), j()) l;
