header a { int<8> b; }
struct c {
  a d;
  bool b;
} control e() {
  apply { c f = {b = false, d = {...}}; }
}
