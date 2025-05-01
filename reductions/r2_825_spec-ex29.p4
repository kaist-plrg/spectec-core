extern void a(in bool b, error c);
header d { bit e; }
header_union f { d g; }
struct h {
  f ip;
} error{i} parser k(h j) {
  state start { a(j.ip.g.e == 4, error.i); }
}
