extern Random<T> {
  Random(T min);
  T read();
}
control c()() {
  Random({#}) r2;
  apply {
    bit<16> v = r2.read();
  }
}
