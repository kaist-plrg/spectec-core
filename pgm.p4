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
action NoAction()
{

}
match_kind {
  exact,
  ternary,
  lpm
}
match_kind {
  range,
  optional,
  selector
}
struct standard_metadata_t {
  ingress_port = bit<9>;
  egress_spec = bit<9>;
  egress_port = bit<9>;
  instance_type = bit<32>;
  packet_length = bit<32>;
  enq_timestamp = bit<32>;
  enq_qdepth = bit<19>;
  deq_timedelta = bit<32>;
  deq_qdepth = bit<19>;
  ingress_global_timestamp = bit<48>;
  egress_global_timestamp = bit<48>;
  mcast_grp = bit<16>;
  egress_rid = bit<16>;
  checksum_error = bit<1>;
  parser_error = error;
  priority = bit<3>;

}
enum CounterType {
  packets,
  bytes,
  packets_and_bytes
}
enum MeterType {
  packets,
  bytes
}
extern counter {
  void count(in bit<32> index);
  counter(dynamic bit<32> size, dynamic enum CounterType type);
}
extern direct_counter {
  void count();
  direct_counter(dynamic enum CounterType type);
}
extern meter {
  void execute_meter<T>(in bit<32> index, out T result);
  meter(dynamic bit<32> size, dynamic enum MeterType type);
}
extern direct_meter<T> {
  void read(out T result);
  direct_meter(dynamic enum MeterType type);
}
extern register<T> {
  void read(out T result, in bit<32> index);
  void write(in bit<32> index, in T value);
  register(dynamic bit<32> size);
}
extern action_profile {
  action_profile(dynamic bit<32> size);
}
extern void random<T>(out T result, in T lo, in T hi);
extern void digest<T>(in bit<32> receiver, in T data);
enum HashAlgorithm {
  crc32,
  crc32_custom,
  crc16,
  crc16_custom,
  random,
  identity,
  csum16,
  xor16
}
extern void mark_to_drop();
extern void mark_to_drop(inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata);
extern void hash<O, T, D, M>(out O result, in enum HashAlgorithm algo, in T base, in D data, in M max);
extern action_selector {
  action_selector(dynamic enum HashAlgorithm algorithm, dynamic bit<32> size, dynamic bit<32> outputWidth);
}
enum CloneType {
  I2E,
  E2E
}
extern Checksum16 {
  bit<16> get<D>(in D data);
  Checksum16();
}
extern void verify_checksum<T, O>(in bool condition, in T data, in O checksum, dynamic enum HashAlgorithm algo);
extern void update_checksum<T, O>(in bool condition, in T data, inout O checksum, dynamic enum HashAlgorithm algo);
extern void verify_checksum_with_payload<T, O>(in bool condition, in T data, in O checksum, dynamic enum HashAlgorithm algo);
extern void update_checksum_with_payload<T, O>(in bool condition, in T data, inout O checksum, dynamic enum HashAlgorithm algo);
extern void resubmit<T>(in T data);
extern void recirculate<T>(in T data);
extern void clone(in enum CloneType type, in bit<32> session);
extern void clone3<T>(in enum CloneType type, in bit<32> session, in T data);
extern void truncate(in bit<32> length);
extern void assert(in bool check);
extern void assume(in bool check);
extern void log_msg(dynamic string msg);
extern void log_msg<T>(dynamic string msg, in T data);
parser Parser<H, M>(dynamic extern packet_in { advance(sizeInBits) : extern_method<>(in sizeInBits bit<32>) -> void
extract(hdr) : extern_method<T>(out hdr T) -> void
extract(variableSizeHeader, variableFieldSizeInBits) : extern_method<T>(out variableSizeHeader T, in variableFieldSizeInBits bit<32>) -> void
length() : extern_method<>() -> bit<32>
lookahead() : extern_method<T>() -> T } b, out H parsedHdr, inout M meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata);
control VerifyChecksum<H, M>(inout H hdr, inout M meta);
control Ingress<H, M>(inout H hdr, inout M meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata);
control Egress<H, M>(inout H hdr, inout M meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } standard_metadata);
control ComputeChecksum<H, M>(inout H hdr, inout M meta);
control Deparser<H>(dynamic extern packet_out { emit(hdr) : extern_method<T>(in hdr T) -> void } b, in H hdr);
package V1Switch<H, M>(dynamic parser Parser(dynamic b extern packet_in { advance(sizeInBits) : extern_method<>(in sizeInBits bit<32>) -> void
extract(hdr) : extern_method<T>(out hdr T) -> void
extract(variableSizeHeader, variableFieldSizeInBits) : extern_method<T>(out variableSizeHeader T, in variableFieldSizeInBits bit<32>) -> void
length() : extern_method<>() -> bit<32>
lookahead() : extern_method<T>() -> T }) p, dynamic control VerifyChecksum(inout hdr H, inout meta M) vr, dynamic control Ingress(inout hdr H, inout meta M, inout standard_metadata struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> }) ig, dynamic control Egress(inout hdr H, inout meta M, inout standard_metadata struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> }) eg, dynamic control ComputeChecksum(inout hdr H, inout meta M) ck, dynamic control Deparser(dynamic b extern packet_out { emit(hdr) : extern_method<T>(in hdr T) -> void }, in hdr H) dep);
const bit<8> MAX_HOPS = 8w10;
const bit<8> STANDARD = 8w0;
const bit<8> HOPS = 8w1;
header type_t {
  tag = bit<8>;

}
header hop_t {
  port = bit<8>;
  bos = bit<8>;

}
header standard_t {
  src = bit<8>;
  dst = bit<8>;

}
struct headers {
  type = header type_t { tag = bit<8> };
  hops = header hop_t { port = bit<8>; bos = bit<8> }[10];
  standard = header standard_t { src = bit<8>; dst = bit<8> };

}
struct metadata {
;

}
parser MyParser(dynamic extern packet_in { advance(sizeInBits) : extern_method<>(in sizeInBits bit<32>) -> void
extract(hdr) : extern_method<T>(out hdr T) -> void
extract(variableSizeHeader, variableFieldSizeInBits) : extern_method<T>(out variableSizeHeader T, in variableFieldSizeInBits bit<32>) -> void
length() : extern_method<>() -> bit<32>
lookahead() : extern_method<T>() -> T } pkt, out struct headers { type = header type_t { tag = bit<8> }; hops = header hop_t { port = bit<8>; bos = bit<8> }[10]; standard = header standard_t { src = bit<8>; dst = bit<8> } } hdr, inout struct metadata {  } meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } std_meta)() {

    state start
      {
        pkt.extract<header type_t { tag = bit<8> }>(hdr.type);
        transition select (hdr.type.tag) {
            (HOPS): parse_hops;
            (STANDARD): parse_standard;
            (default): accept;
          };
      }
    state parse_hops
      {
        pkt.extract<header hop_t { port = bit<8>; bos = bit<8> }>(hdr.hops.next);
        transition select (hdr.hops.last.bos) {
            (8w1): parse_standard;
            (default): parse_hops;
          };
      }
    state parse_standard
      {
        pkt.extract<header standard_t { src = bit<8>; dst = bit<8> }>(hdr.standard);
        transition accept;
      }
}
control MyVerifyChecksum(inout struct headers { type = header type_t { tag = bit<8> }; hops = header hop_t { port = bit<8>; bos = bit<8> }[10]; standard = header standard_t { src = bit<8>; dst = bit<8> } } hdr, inout struct metadata {  } meta)() {

  apply
  {

  }
}
control MyComputeChecksum(inout struct headers { type = header type_t { tag = bit<8> }; hops = header hop_t { port = bit<8>; bos = bit<8> }[10]; standard = header standard_t { src = bit<8>; dst = bit<8> } } hdr, inout struct metadata {  } meta)() {

  apply
  {

  }
}
control MyIngress(inout struct headers { type = header type_t { tag = bit<8> }; hops = header hop_t { port = bit<8>; bos = bit<8> }[10]; standard = header standard_t { src = bit<8>; dst = bit<8> } } hdr, inout struct metadata {  } meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } std_meta)() {
  action allow()
  {

  }
  action deny()
  {
    std_meta.egress_spec = 9w511;
  }
  apply
  {
    std_meta.egress_spec = ((bit<9>) (hdr.hops[0].port));
    hdr.hops.pop_front(1);
    if (!hdr.hops[0].isValid())
    {
      hdr.type.tag = 8w0;
    }
  }
}
control MyEgress(inout struct headers { type = header type_t { tag = bit<8> }; hops = header hop_t { port = bit<8>; bos = bit<8> }[10]; standard = header standard_t { src = bit<8>; dst = bit<8> } } hdr, inout struct metadata {  } meta, inout struct standard_metadata_t { ingress_port = bit<9>; egress_spec = bit<9>; egress_port = bit<9>; instance_type = bit<32>; packet_length = bit<32>; enq_timestamp = bit<32>; enq_qdepth = bit<19>; deq_timedelta = bit<32>; deq_qdepth = bit<19>; ingress_global_timestamp = bit<48>; egress_global_timestamp = bit<48>; mcast_grp = bit<16>; egress_rid = bit<16>; checksum_error = bit<1>; parser_error = error; priority = bit<3> } std_meta)() {

  apply
  {

  }
}
control MyDeparser(dynamic extern packet_out { emit(hdr) : extern_method<T>(in hdr T) -> void } pkt, in struct headers { type = header type_t { tag = bit<8> }; hops = header hop_t { port = bit<8>; bos = bit<8> }[10]; standard = header standard_t { src = bit<8>; dst = bit<8> } } hdr)() {

  apply
  {
    pkt.emit<header type_t { tag = bit<8> }>(hdr.type);
    pkt.emit<header hop_t { port = bit<8>; bos = bit<8> }[10]>(hdr.hops);
    pkt.emit<header standard_t { src = bit<8>; dst = bit<8> }>(hdr.standard);
  }
}
