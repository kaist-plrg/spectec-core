# --------------------------------------
# Stage 1: P4C dependencies
# --------------------------------------
FROM p4lang/behavioral-model:latest AS base 

ARG DEBIAN_FRONTEND=noninteractive
# ENV TZ=Asia/Seoul
ARG ENABLE_BMV2=OFF
ARG ENABLE_GTESTS=ON
ARG ENABLE_WERROR=ON

RUN apt-get update && \
    apt-get install -y \
    sudo bison build-essential ccache cmake curl flex g++ git lld libboost-dev libboost-graph-dev \
    libboost-iostreams-dev libfl-dev ninja-build pkg-config python3 python3-pip python3-setuptools tcpdump \
    wget ca-certificates \
    && apt-get clean

COPY p4c /home/p4c
WORKDIR /home/p4c
RUN pip3 install --upgrade pip && \
    pip3 install -r requirements.txt

# Build
WORKDIR /home/p4c/build
RUN ccache --set-config=max_size=1G
ENV CMAKE_FLAGS="-DCMAKE_UNITY_BUILD=OFF \
    -DENABLE_GTESTS=${ENABLE_GTESTS} \
    -DCMAKE_BUILD_TYPE=Debug \
    -DENABLE_WERROR=${ENABLE_WERROR} \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
RUN cmake $CMAKE_FLAGS -G "Unix Makefiles" .. \
    -DCMAKE_CXX_FLAGS="--coverage -O0" \
    -DCMAKE_C_FLAGS="--coverage -O0"
RUN cmake --build . -- -j$(nproc) VERBOSE=1 && \
    cmake --install . && \
    ccache -p -s;

RUN pip3 install gcovr

COPY run_coverage.sh /home/run_coverage.sh
RUN chmod a+x /home/run_coverage.sh
WORKDIR /home

# --------------------------------------
# Stage 2: Copy p4spectec
# --------------------------------------
FROM base AS p4base

COPY . /home/p4spectec
# RUN rm -rf /home/p4spectec/p4c && \
#     mv /home/p4c /home/p4spectec

WORKDIR /home/p4spectec

# ---------------------------------------
# Stage 3: Installations - p4cherry/p4spec
# ---------------------------------------
FROM p4base AS opambase

RUN apt-get update && \
    apt-get install -y opam make libgmp-dev pkg-config && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Initialize opam
RUN opam init --disable-sandboxing --auto-setup && \
    opam switch create 4.14.0 && \
    eval $(opam env) && \
    opam install dune menhir bignum core.v0.15.1 core_unix.v0.15.2 bisect_ppx -y

# Set opam environment permanently
ENV OPAM_SWITCH_PREFIX=/root/.opam/4.14.0
ENV PATH=$OPAM_SWITCH_PREFIX/bin:$PATH
ENV CAML_LD_LIBRARY_PATH=$OPAM_SWITCH_PREFIX/lib/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml/stublibs:$OPAM_SWITCH_PREFIX/lib/ocaml

# ---------------------------------------
# Stage 4: Build p4spec
# ---------------------------------------
FROM opambase AS p4specbase

RUN make build-spec && \
    chmod a+x ./p4spectec

# --------------------------------------
# Stage 5: Fuzzer & Reducer dependencies
# --------------------------------------
FROM p4specbase AS reducebase

RUN apt-get update && \
    apt-get install -y clang creduce python3 pip && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install psutil
COPY patches/creduce /usr/bin/creduce
RUN chmod +x /usr/bin/creduce

ENV P4CHERRY_PATH=/home/p4spectec
