# --------------------------------------
# Stage 1: Build P4C
# --------------------------------------
FROM p4lang/behavioral-model:latest AS base 

ARG DEBIAN_FRONTEND=noninteractive
ARG ENABLE_BMV2=ON
ARG ENABLE_EBPF=ON
ARG ENABLE_GTESTS=ON
ARG ENABLE_WERROR=ON

RUN apt-get update && \
    apt-get install -y \
    sudo bison build-essential cmake curl flex g++ git lld libboost-dev libboost-graph-dev \
    libboost-iostreams-dev libfl-dev ninja-build pkg-config python3 python3-pip python3-setuptools tcpdump \
    wget ca-certificates

RUN echo "deb http://download.opensuse.org/repositories/home:/p4lang/xUbuntu_${DISTRIB_RELEASE}/ /" | sudo tee /etc/apt/sources.list.d/home:p4lang.list && \
    curl -fsSL https://download.opensuse.org/repositories/home:p4lang/xUbuntu_${DISTRIB_RELEASE}/Release.key | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/home_p4lang.gpg > /dev/null
    P4C_RUNTIME_DEPS+=" p4lang-bmv2"

# BMV2 runtime deps
RUN apt-get update && \
    apt-get install -y \
    p4lang-bmv2 cpp libboost-graph1.7* libboost-iostreams1.7* libgc1* libgmp-dev libnanomsg-dev

# EBPF runtime deps
RUN apt-get update && \
    apt-get install -y \
    libpcap-dev libelf-dev zlib1g-dev llvm \
    clang iproute2 iptables net-tools

RUN apt-get update && \
    apt-get install -y \
    libpcap-dev libelf-dev zlib1g-dev gcc-multilib net-tools \
    xtables-addons-source python3-argcomplete

RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 15 && \
    rm llvm.sh

RUN git clone https://github.com/libbpf/libbpf/ -b v1.5.0 ${P4C_DIR}/backends/tc/runtime/libbpf
  ${P4C_DIR}/backends/tc/runtime/build-libbpf

COPY . /home/p4spectec
WORKDIR /home/p4spectec/p4c
RUN pip3 install --upgrade pip && \
    pip3 install nnpy && \
    pip3 install -r requirements.txt

# Build
WORKDIR /home/p4spectec/p4c/build
# RUN ccache --set-config=max_size=1G
ENV CMAKE_FLAGS="-DCMAKE_UNITY_BUILD=OFF \
    -DENABLE_GTESTS=${ENABLE_GTESTS} \
    -DCMAKE_BUILD_TYPE=Debug \
    -DENABLE_WERROR=${ENABLE_WERROR} \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
RUN cmake $CMAKE_FLAGS -G "Unix Makefiles" .. \
    -DCMAKE_CXX_FLAGS="--coverage -O0" \
    -DCMAKE_C_FLAGS="--coverage -O0"
RUN cmake --build . -- -j$(nproc) VERBOSE=1 && \
    cmake --install . 

RUN pip3 install gcovr
WORKDIR /home/p4spectec

