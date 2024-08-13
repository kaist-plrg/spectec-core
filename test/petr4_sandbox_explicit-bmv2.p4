#include <core.p4>
#include <v1model.p4>

const bit<8> MAX_HOPS = 8w10;
const bit<8> STANDARD = 8w0;
const bit<8> HOPS = 8w1;

header type_t {
  bit<8> tag;
}

header hop_t {
  bit<8> port;
  bit<8> bos;
}

header standard_t {
  bit<8> src;
  bit<8> dst;
}

struct headers {
  type_t type;
  hop_t[MAX_HOPS] hops;
  standard_t standard;
}

struct metadata { }

parser MyParser(packet_in pkt, out headers hdr, inout metadata meta, inout standard_metadata_t std_meta) {
    state start {
        pkt.extract<type_t>(hdr.type);
        transition select(hdr.type.tag) {
            HOPS: parse_hops;
            STANDARD: parse_standard;
            default: accept;
        }
    }
    state parse_hops {
        pkt.extract<hop_t>(hdr.hops.next);
        transition select(hdr.hops.last.bos) {
            8w1: parse_standard;
            default: parse_hops;
        }
    }
    
    state parse_standard {
        pkt.extract<standard_t>(hdr.standard);
        transition accept;
    }
}

control MyVerifyChecksum(inout headers hdr, inout metadata meta) {
    apply { }
}

control MyComputeChecksum(inout headers hdr, inout metadata meta) {
    apply { }
}

control MyIngress(inout headers hdr, inout metadata meta, inout standard_metadata_t std_meta) {
  action allow() { }
  action deny() { std_meta.egress_spec = 9w511; }

  table acl {
    key = { hdr.standard.src : exact; hdr.standard.dst : exact; }
    actions = { allow; deny; }
    const entries = { (0xCC, 0xDD) : deny(); }
    default_action = allow();
  }
  apply {
    std_meta.egress_spec = (bit<9>) hdr.hops[0].port;
    hdr.hops.pop_front(1);
    if (!hdr.hops[0].isValid()) {
        hdr.type.tag = 8w0;
    }
    acl.apply();
  }
}

control MyEgress(inout headers hdr, inout metadata meta, inout standard_metadata_t std_meta) {
    apply { }
}

control MyDeparser(packet_out pkt, in headers hdr) {
    apply {
        pkt.emit<type_t>(hdr.type);
        pkt.emit<hop_t[MAX_HOPS]>(hdr.hops);
        pkt.emit<standard_t>(hdr.standard);
    }
}

V1Switch<headers, metadata>(MyParser(), MyVerifyChecksum(), MyIngress(), MyEgress(), MyComputeChecksum(), MyDeparser()) main;
