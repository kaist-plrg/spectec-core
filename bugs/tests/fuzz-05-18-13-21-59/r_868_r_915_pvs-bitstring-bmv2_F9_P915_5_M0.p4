extern packet_in {
}
          struct standard_metadata_t {
}
          struct my_packet {
}
          struct my_metadata {
}
          parser MyParser(packet_in b, out my_packet p, inout my_metadata m, inout standard_metadata_t s) {
          state start {             transition b;           }
        }
