extern a {
  a();
  abstract bit<16> b(in bit<16> d);
}
control c() { a() cntr = {bit<16> b(in bit<16> d){return d; }
}
;
apply {}
}
