error {
  ParserInvalidArgument
}
match_kind {
  exact,
  bytes
};
enum HashAlgorithm {
  crc32,
  I2E,
};
control VerifyChecksum<H, M>(error a = {#}, inout M meta);
header ethernet_t {
}
error {
  IPv4ChecksumError
}
