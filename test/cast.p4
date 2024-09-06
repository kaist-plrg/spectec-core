typedef bit<8> BIT8;

enum BIT8 ENUM8 {
  a = 0,
  b = 1
};

header hdr_t {
    bit<8> x;
}

struct str_t {
    hdr_t h;
}

header hdr2_t {
    bit<4> y;
}

struct str2_t {
    hdr2_t h;
}

control c(in int z) {
  apply {
    bit<8> x = 0;
    BIT8 y = 1;
    bit<8> z = ENUM8.a + 1;
    str2_t s2 = { h = { y = 0 } };
    //str_t s1 = (str_t) s2;
  }
}
