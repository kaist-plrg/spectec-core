    extern packet_in {
       }
        struct standard_metadata_t {
       }
        struct headers_t {
       }
        struct metadata_t {
       }
        parser EmptyParser(packet_in b, out headers_t headers, inout metadata_t meta, inout standard_metadata_t standard_metadata)() {
         state start {
            transition b;
          }
       }
