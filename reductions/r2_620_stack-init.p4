header a<b> {}
control c() {
  apply { a<bit<32>>[3] d = (a<bit<32>>[3]){}; }
}
