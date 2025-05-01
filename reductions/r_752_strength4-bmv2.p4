struct a {
  bit b;
  bit<16> e;
} control c(inout a d) {
  apply { d.e = (16w0 ++d.b)[15:0]; }
}
