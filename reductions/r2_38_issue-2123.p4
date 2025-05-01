enum a { b } extern void c<d, e>(in d data, e checksum);
control f() {
  apply { c(data = {}, checksum = a.b); }
}
