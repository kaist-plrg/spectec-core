#include <core.p4>
#include <v1model.p4>

header hdr {
    bit<8>  e;
    bit<16> t;
    bit<8>  l;
    bit<8> r;
    bit<8>  v;
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
    action to_0()
    { h.h.r = (bit<8>)(0); sm.egress_spec = 0; }
    action to_1()
    { h.h.r = (bit<8>)(1); sm.egress_spec = 1; }
    action to_2()
    { h.h.r = (bit<8>)(2); sm.egress_spec = 2; }
    action to_3()
    { h.h.r = (bit<8>)(3); sm.egress_spec = 3; }
    action to_4()
    { h.h.r = (bit<8>)(4); sm.egress_spec = 4; }

    table t {
        key = {
            h.h.e : exact;
            h.h.t : ternary;
        }
        actions = {
            to_0;
            to_1;
            to_2;
            to_3;
            to_4;
        }
        const entries = {
            (0x01, 0x1111 &&& 0xF   ) : to_1;
            (0x02, 0x1181           ) : to_2;
            (0x03, 0x1111 &&& 0xF000) : to_3;
            (0x04, _                ) : to_4;
        }
        const default_action = to_0;
    }
    apply { t.apply(); }
}

control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<hdr>(h.h); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
