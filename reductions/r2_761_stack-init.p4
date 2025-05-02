header a<d> { bit<32> b; }
control c(out bit<32> e) {
  apply {
    a<bit>[3] f;
    e = f[2].b;
  }
}
