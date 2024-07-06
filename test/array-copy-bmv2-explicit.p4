#include <core.p4>
#include <v1model.p4>

header hdr {
    bit<8> x;
}

struct headers {
    hdr[2] h1;
    hdr[2] h2;
}

struct metadata {}

parser P(packet_in p, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        p.extract<hdr>(h.h1.next);
        p.extract<hdr>(h.h1.next);
        h.h2 = h.h1;
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }
control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<headers>(h); }
}

control ingress(inout headers h, inout metadata m, inout standard_metadata_t sm) {
    apply {
        sm.egress_spec = 0;
    }
}

V1Switch(P(), vrfy(), ingress(), egress(), update(), deparser()) main;
