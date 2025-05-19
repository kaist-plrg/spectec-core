 @metadata @name("standard_metadata") struct standard_metadata_t {
  }
   extern register<T> {
      register(bit<32> size);
  }
   header ethernet_t {
  }
   struct headers_t {
  }
   struct reg_data2_t {
  }
   struct metadata_t {
  }
   control ingress(inout headers_t hdr,                 inout metadata_t meta,                 inout standard_metadata_t stdmeta) {
      register<bit<8> >(16) reg1;
      apply {
     }
  }
