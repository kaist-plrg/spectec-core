#include <core.p4>
#include <v1model.p4>

header hdr {
  bit<8> a;
  bit<8> b;
}

struct Headers {
  hdr h;
}

struct Meta {}

parser p(packet_in b, out Headers h, inout Meta m, inout standard_metadata_t sm) {
  state start {
    b.extract(h.h);
    transition accept;
  }
}

control vr(inout Headers h, inout Meta m) { apply {} }

bit<8> f(in bool b) {
  return 8w42;
}

control ig(inout Headers h, inout Meta m, inout standard_metadata_t sm) {
  action foo() { exit; }
  table t {
    actions = { foo; }
    const default_action = foo;
  }
  apply {
    h.h.a = f(t.apply().hit);
  }
}

control eg(inout Headers h, inout Meta m, inout standard_metadata_t sm) { apply {} }
control ck(inout Headers h, inout Meta m) { apply {} }

control dep(packet_out b, in Headers h) {
  apply { b.emit(h.h); }
}

V1Switch(p(), vr(), ig(), eg(), ck(), dep()) main;
