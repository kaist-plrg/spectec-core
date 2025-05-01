header_union a {}
struct b {
  a d;
} control c(in b e) {
  apply {}
}
control f(in b e);
package g(f c);
g(c()) h;
