header a {}
header_union b { a d; }
control c() {
  action e(b[2] f) {
    bit g;
    f[g].d.setInvalid();
  }
  apply {}
}
