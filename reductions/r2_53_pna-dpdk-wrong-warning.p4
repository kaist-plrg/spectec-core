header_union a {}
control b() {
  a[2] c;
  a[2] d;
  apply { c = d; }
}
