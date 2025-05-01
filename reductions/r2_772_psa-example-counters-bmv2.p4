typedef bit a;
enum b { k } extern c<d, e> { c(bit<32> f, b g); }
typedef bit h;
const bit<32> i = 2;
control ingress() {
  c<h, a>(i, b.k) j;
  apply {}
}
