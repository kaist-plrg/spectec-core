extern a {}
control b(a c) {
  apply {}
}
control ctr(a c);
package d(ctr e);
d(b()) f;
