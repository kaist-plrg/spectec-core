extern a<b> {
  a(b d);
  b e();
}
control c() {
  a(6) f;
  apply { f.e(); }
}
