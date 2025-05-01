struct c {
  bit d;
  bit<16> e;
} enum bit<16> f{g = 0} header h {
  f l;
}
struct i {
  h[2] j;
} control k(i a, inout c b) {
  action forward() { b.e = (bit<16>)a.j[b.d].l [11:8]; }
  apply {}
}
