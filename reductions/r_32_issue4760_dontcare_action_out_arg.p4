struct a {
} control b(inout a c) {
  apply {}
}
control g(inout a c);
package d(g e);
d(b()) f;
