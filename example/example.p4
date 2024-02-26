parser p<T> (in bit<8> i);
extern E {
  D foo<D>(bit<555> x);
}
parser TP(in bit<8> i) {
  state start {
    transition accept;
  }
}
