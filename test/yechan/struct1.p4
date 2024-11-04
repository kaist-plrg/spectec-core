#include <core.p4>
#include <v1model.p4>

// Generic structure type
struct S<T> {
    T field;
    bool valid;
}

struct G<T> {
    S<T> s;
}

// Specialize S by replacing 'T' with 'bit<32>'
const S<bit<32>> s = { 
    field = 32w15, 
    valid = false 
};

// Specialize G by replacing 'T' with 'bit<32>'
const G<bit<32>> g = { 
    s = { 
        field = 25, 
        valid = false 
    } 
};

// Generic header type
header hdr<T> {
  T a;
  T b;
  T c;
}

// Specialize H by replacing 'T' with 'bit<8>'
const hdr<bit<8>> h = { 
    a = 1,
    b = 1,
    c = 1
};

// Header stack produced from a specialization of a generic header type
// TODO : H<bit<8>>[10] stack;

// Generic header union
header_union HU<T> {
    hdr<bit<32>> h32;
    hdr<bit<8>> h8;
    hdr<T> ht;
}

// Header union with a type obtained by specializing a generic header union type
// HU<bit<32>> hu;
const int<32> x = s.field;
const int<32> y = g.s.field;

const int<32> w = 10;

struct headers {
    HU<bit<4>> hu;
}

struct metadata {}

parser p(packet_in b, out headers h, inout metadata m, inout standard_metadata_t sm) {
    state start {
        b.extract<hdr<bit<4>>>(h.hu.ht);
        transition accept;
    }
}

control vrfy(inout headers h, inout metadata m) { apply {} }

control ingress(inout headers h, inout metadata m, inout standard_metadata_t sm) {
    action add()
    { h.hu.ht.c = (bit<4>)(h.hu.ht.a + h.hu.ht.b); sm.egress_spec = x; }
    action sub()
    { h.hu.ht.c = (bit<4>)(h.hu.ht.a - h.hu.ht.b); sm.egress_spec = y; }
    action a()
    { h.hu.ht.c = (bit<4>)(h.hu.ht.a + h.hu.ht.b); sm.egress_spec = w; }

    table t {
        key = { h.hu.ht.a : exact; h.hu.ht.b : exact; h.hu.ht.c : exact; }
        actions = { add; sub; a;}
        const entries = {
          (32w0, 32w1, 32w2) : add;
          (32w7, 32w7, 32w7) : sub;
        }
        const default_action = a;
    }
    apply { t.apply(); }
}

control update(inout headers h, inout metadata m) { apply {} }

control egress(inout headers h, inout metadata m, inout standard_metadata_t sm) { apply {} }

control deparser(packet_out b, in headers h) {
    apply { b.emit<hdr<bit<4>>>(h.hu.ht); }
}

V1Switch(p(), vrfy(), ingress(), egress(), update(), deparser()) main;
