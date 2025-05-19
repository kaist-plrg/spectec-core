match_kind {
  selector
}
struct standard_metadata_t {
}
enum CounterType {
  packets,
};
enum MeterType {
  packets,
}
extern meter {
  meter(bit<32> size, MeterType type = {#});
}
enum HashAlgorithm {
  crc32,
};
struct headers {
}
struct metadata {
}
error {
  IPv4ChecksumError
}
control cEgress(inout headers hdr, inout metadata meta, inout standard_metadata_t stdmeta)() {
  apply {}
}
