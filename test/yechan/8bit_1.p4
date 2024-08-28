#include <core.p4>
#include <v1model.p4>

header hdr {
  bit<8> a;
  bit<8> b;
  bit<8> c;
  bit<16> d;
}

struct headers {
    hdr h;
}

struct metadata {}

parser p(packet_in b, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        b.extract<hdr>(h.h);
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }

control ingress(inout headers h, inout metadata m, inout standard_metadata_t sm) {
    action add()
    { h.h.d = (bit<16>)(h.h.a + h.h.b + h.h.c); sm.egress_spec = 0; }
    action sub()
    { h.h.c = (bit<8>)(h.h.b - h.h.a); sm.egress_spec = 1; }

    table t {
        key = { h.h.a : exact; h.h.b : exact; h.h.c : exact; h.h.d : exact;}
        actions = { add; sub; }
        const entries = {
          (32w0, 32w1, 32w2, 32w14) : sub;
          (32w7, 32w7, 32w7, 32w49) : sub;
          (32w1, 32w2, 32w3, _) : sub;
          
        }
        const default_action = add;
    }
    apply { t.apply(); }
}

control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<hdr>(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
