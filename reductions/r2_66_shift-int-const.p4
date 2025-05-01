header a {}
control b(inout a c) {
  apply {}
}
control f(inout a c);
package d(f b);
d(b()) e;
