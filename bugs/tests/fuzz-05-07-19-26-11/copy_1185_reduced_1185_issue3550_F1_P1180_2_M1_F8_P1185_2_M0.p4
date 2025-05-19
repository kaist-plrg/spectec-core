// Intended pid 1185
// Source vid 1588
// Depth 2

// Mutation GenFromTyp

/*
From [ (ExprK (BoolE true)),
  (ExprK (NumE (FBIT 48 +1))),
  (ExprK (NumE (FBIT 48 +2))),
  (ExprK (TypeAccE (CURRENT "Proto") "proto1")),
  (ExprK (MaskE (NumE (FBIT 48 +2)) (NumE (FBIT 48 +3)))),
  (ExprK (RangeE (NumE (FBIT 48 +2)) (NumE (FBIT 48 +4)))),
  (DefaultK) ]
To [ (ExprK (BoolE true)),
  (ExprK (NumE (FBIT 48 +1))),
  (ExprK (NumE (FBIT 48 +2))),
  (DefaultK),
  (ExprK (MaskE (NumE (FBIT 48 +2)) (NumE (FBIT 48 +3)))),
  (ExprK (RangeE (NumE (FBIT 48 +2)) (NumE (FBIT 48 +4)))),
  (DefaultK) ]
*/

error {
  NoError,
  PacketTooShort,
  NoMatch,
  StackOutOfBounds,
  HeaderTooShort,
  ParserTimeout,
  ParserInvalidArgument
}
extern packet_in {
  void extract<T>(out T hdr);
  void extract<T>(out T variableSizeHeader, in bit<32> variableFieldSizeInBits);
  T lookahead<T>();
  void advance(in bit<32> sizeInBits);
  bit<32> length();
}
extern packet_out {
  void emit<T>(in T hdr);
}
extern void verify(in bool check, in error toSignal);
action NoAction() {}
match_kind {
  exact,
  ternary,
  lpm
}
extern bool static_assert(bool check, string message);
extern bool static_assert(bool check);
typedef bit<32> PortIdUint_t;
typedef bit<32> MulticastGroupUint_t;
typedef bit<16> CloneSessionIdUint_t;
typedef bit<8> ClassOfServiceUint_t;
typedef bit<16> PacketLengthUint_t;
typedef bit<16> EgressInstanceUint_t;
typedef bit<64> TimestampUint_t;
type PortIdUint_t PortId_t;
type MulticastGroupUint_t MulticastGroup_t;
type CloneSessionIdUint_t CloneSessionId_t;
type ClassOfServiceUint_t ClassOfService_t;
type PacketLengthUint_t PacketLength_t;
type EgressInstanceUint_t EgressInstance_t;
type TimestampUint_t Timestamp_t;
typedef error ParserError_t;
const PortId_t PSA_PORT_RECIRCULATE = ((PortId_t) (4294967290));
const PortId_t PSA_PORT_CPU = ((PortId_t) (4294967293));
const CloneSessionId_t PSA_CLONE_SESSION_TO_CPU = ((CloneSessionId_t) (0));
typedef bit<32> PortIdInHeaderUint_t;
typedef bit<32> MulticastGroupInHeaderUint_t;
typedef bit<16> CloneSessionIdInHeaderUint_t;
typedef bit<8> ClassOfServiceInHeaderUint_t;
typedef bit<16> PacketLengthInHeaderUint_t;
typedef bit<16> EgressInstanceInHeaderUint_t;
typedef bit<64> TimestampInHeaderUint_t;
type PortIdInHeaderUint_t PortIdInHeader_t;
type PacketLengthInHeaderUint_t PacketLengthInHeader_t;
type EgressInstanceInHeaderUint_t EgressInstanceInHeader_t;
type TimestampInHeaderUint_t TimestampInHeader_t;
TimestampInHeader_t psa_Timestamp_int_to_header(in Timestamp_t x) {
  return ((TimestampInHeader_t) (((TimestampInHeaderUint_t) (((TimestampUint_t) (x))))));
}
enum PSA_IdleTimeout_t {
  NO_TIMEOUT,
  NOTIFY_CONTROL
};
enum PSA_PacketPath_t {
  NORMAL,
  NORMAL_UNICAST,
  NORMAL_MULTICAST,
  CLONE_I2E,
  CLONE_E2E,
  RESUBMIT,
  RECIRCULATE
};
struct psa_ingress_parser_input_metadata_t {
  PortId_t ingress_port;
  PSA_PacketPath_t packet_path;

}
struct psa_egress_parser_input_metadata_t {
  PortId_t egress_port;
  PSA_PacketPath_t packet_path;

}
struct psa_ingress_input_metadata_t {
  PortId_t ingress_port;
  PSA_PacketPath_t packet_path;
  Timestamp_t ingress_timestamp;
  ParserError_t parser_error;

}
struct psa_ingress_output_metadata_t {
  ClassOfService_t class_of_service;
  bool clone;
  CloneSessionId_t clone_session_id;
  bool drop;
  bool resubmit;
  MulticastGroup_t multicast_group;
  PortId_t egress_port;

}
struct psa_egress_input_metadata_t {
  ClassOfService_t class_of_service;
  PortId_t egress_port;
  PSA_PacketPath_t packet_path;
  EgressInstance_t instance;
  Timestamp_t egress_timestamp;
  ParserError_t parser_error;

}
struct psa_egress_deparser_input_metadata_t {
  PortId_t egress_port;

}
struct psa_egress_output_metadata_t {
  bool clone;
  CloneSessionId_t clone_session_id;
  bool drop;

}
match_kind {
  range,
  selector,
  optional
}
action send_to_port(inout psa_ingress_output_metadata_t meta, in PortId_t egress_port) {
  meta.drop = false;
  meta.multicast_group = ((MulticastGroup_t) (0));
  meta.egress_port = egress_port;
}
extern PacketReplicationEngine {
  PacketReplicationEngine();
}
extern BufferingQueueingEngine {
  BufferingQueueingEngine();
}
enum PSA_HashAlgorithm_t {
  IDENTITY,
  CRC32,
  CRC32_CUSTOM,
  CRC16,
  CRC16_CUSTOM,
  ONES_COMPLEMENT16,
  TARGET_DEFAULT
};
extern ActionSelector {
  ActionSelector(PSA_HashAlgorithm_t algo, bit<32> size, bit<32> outputWidth);
}
extern Digest<T> {
  Digest();
  void pack(in T data);
}
parser IngressParser<H, M, RESUBM, RECIRCM>(packet_in buffer, out H parsed_hdr, inout M user_meta, in psa_ingress_parser_input_metadata_t istd, in RESUBM resubmit_meta, in RECIRCM recirculate_meta);
control Ingress<H, M>(inout H hdr, inout M user_meta, in psa_ingress_input_metadata_t istd, inout psa_ingress_output_metadata_t ostd);
control IngressDeparser<H, M, CI2EM, RESUBM, NM>(packet_out buffer, out CI2EM clone_i2e_meta, out RESUBM resubmit_meta, out NM normal_meta, inout H hdr, in M meta, in psa_ingress_output_metadata_t istd);
parser EgressParser<H, M, NM, CI2EM, CE2EM>(packet_in buffer, out H parsed_hdr, inout M user_meta, in psa_egress_parser_input_metadata_t istd, in NM normal_meta, in CI2EM clone_i2e_meta, in CE2EM clone_e2e_meta);
control Egress<H, M>(inout H hdr, inout M user_meta, in psa_egress_input_metadata_t istd, inout psa_egress_output_metadata_t ostd);
control EgressDeparser<H, M, CE2EM, RECIRCM>(packet_out buffer, out CE2EM clone_e2e_meta, out RECIRCM recirculate_meta, inout H hdr, in M meta, in psa_egress_output_metadata_t istd, in psa_egress_deparser_input_metadata_t edstd);
package IngressPipeline<IH, IM, NM, CI2EM, RESUBM, RECIRCM>(IngressParser<IH, IM, RESUBM, RECIRCM> ip, Ingress<IH, IM> ig, IngressDeparser<IH, IM, CI2EM, RESUBM, NM> id);
package EgressPipeline<EH, EM, NM, CI2EM, CE2EM, RECIRCM>(EgressParser<EH, EM, NM, CI2EM, CE2EM> ep, Egress<EH, EM> eg, EgressDeparser<EH, EM, CE2EM, RECIRCM> ed);
package PSA_Switch<IH, IM, EH, EM, NM, CI2EM, CE2EM, RESUBM, RECIRCM>(IngressPipeline<IH, IM, NM, CI2EM, RESUBM, RECIRCM> ingress, PacketReplicationEngine pre, EgressPipeline<EH, EM, NM, CI2EM, CE2EM, RECIRCM> egress, BufferingQueueingEngine bqe);
typedef bit<48> EthernetAddress;
header ethernet_t {
  EthernetAddress dstAddr;
  EthernetAddress srcAddr;
  bit<16> etherType;

}
header ipv4_t {
  bit<4> version;
  bit<4> ihl;
  bit<8> diffserv;
  bit<16> totalLen;
  bit<16> identification;
  bit<3> flags;
  bit<13> fragOffset;
  bit<8> ttl;
  bit<8> protocol;
  bit<16> hdrChecksum;
  bit<32> srcAddr;
  bit<32> dstAddr;
  bit<80> newfield;

}
header tcp_t {
  bit<16> srcPort;
  bit<16> dstPort;
  bit<32> seqNo;
  bit<32> ackNo;
  bit<4> dataOffset;
  bit<3> res;
  bit<3> ecn;
  bit<6> ctrl;
  bit<16> window;
  bit<16> checksum;
  bit<16> urgentPtr;

}
enum bit<8> Proto {
proto1 = 1,
proto2 = 2
};
struct empty_metadata_t {

}
struct metadata {
  bit<16> data;
  bit<48> key1;
  bit<48> key2;
  bit<48> key3;
  bit<48> key4;

}
struct headers {
  ethernet_t ethernet;
  ipv4_t ipv4;
  tcp_t tcp;

}
control ingress(inout headers hdr, inout metadata user_meta, in psa_ingress_input_metadata_t istd, inout psa_ingress_output_metadata_t ostd)() {
  action execute(bit<48> x) {
    user_meta.data = 1;
  }
  table tbl {
    key = {
      hdr.ethernet.isValid() : exact;
      hdr.ethernet.dstAddr : exact;
      hdr.ethernet.srcAddr : exact;
      hdr.ipv4.protocol : exact;
      user_meta.key1 : ternary;
      user_meta.key2 : range;
      user_meta.key4 : optional;
    }
    actions = {
      NoAction;
      execute;
    }
    const entries = {
      (true, 48w1, 48w2, default, 48w2 &&& 48w3, 48w2 .. 48w4, default) : execute(48w1);
      (true, 48w1, 48w2, Proto.proto1, 48w2 &&& 48w3, 48w2 .. 48w5, 48w10) : execute(48w1);
      (true, 48w3, 48w3, Proto.proto1, 48w2 &&& 48w3, 48w2 .. 48w6, 48w10) : execute(48w1);
    }
  }
  table tbl1 {
    key = {
      hdr.ethernet.isValid() : exact;
      hdr.ethernet.dstAddr : exact;
      hdr.ethernet.srcAddr : exact;
      user_meta.key3 : lpm;
    }
    actions = {
      NoAction;
      execute;
    }
    const entries = {
      (true, 48w1, 48w2, 48w10) : execute(48w1);
      (true, 48w1, 48w2, 48w11) : execute(48w1);
      (true, 48w3, 48w3, 48w12) : execute(48w1);
    }
  }
  apply {
    tbl.apply();
    tbl1.apply();
  }
}

// Covered pids { 1185 }
