extern crc_poly<O> {
  crc_poly(O poly);
  O hash<T>(in T data);
}
header h1_t {
}
struct hdrs {
  bit<16> crc;
}
control test(inout hdrs hdr)() {
  apply {
    hdr.crc = crc_poly({#}).hash(hdr.h1);
  }
}
