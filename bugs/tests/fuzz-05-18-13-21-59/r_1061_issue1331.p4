 extern packet_in {
    }
     header ethernet {
    }
     parser P(packet_in pkt);
     control MyC(packet_in pkt, inout ethernet ether)(P p) {
        apply {
           p.apply(pkt);
       }
    }
