# --------------------------------------
# Stage 1: System dependencies
# --------------------------------------
FROM ubuntu:20.04 AS base

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Seoul

RUN apt-get update && \
    apt-get install -y git make curl && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /home

# --------------------------------------
# Stage 2: Clone repo
# --------------------------------------
FROM base AS source

RUN git clone https://github.com/kaist-plrg/p4cherry.git && \
    cd p4cherry && \
    git checkout p4spec-sl-mod-il && \
    git submodule update --init --recursive

WORKDIR /home/p4cherry

# ---------------------------------------
# Stage 3: Installations - p4cherry/p4spec
# ---------------------------------------
FROM source AS opambase

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Seoul

RUN apt-get update && \
    apt-get install -y opam libgmp-dev pkg-config && \
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
# Stage 5: Reducer dependencies
# --------------------------------------
FROM p4specbase AS reducebase

RUN apt-get update && \
    apt-get install -y clang creduce python3 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY patches/creduce /usr/bin/creduce
RUN chmod +x /usr/bin/creduce

ENV P4CHERRY_PATH=/home/p4cherry

# --------------------------------------
# Stage 6: P4C dependencies
# --------------------------------------
FROM p4lang/behavioral-model:latest

ARG MAKEFLAGS=-j2
ARG IN_DOCKER=TRUE
ARG IMAGE_TYPE=test
# No questions asked during package installation.
ARG DEBIAN_FRONTEND=noninteractive
# Whether to do a unity build.
ARG CMAKE_UNITY_BUILD=OFF
# Whether to enable translation validation
ARG VALIDATION=OFF
# This creates a release build that includes link time optimization and links
# all libraries except for glibc statically.
ARG STATIC_BUILD_WITH_DYNAMIC_GLIBC=OFF
# This creates a release build that includes link time optimization and links
# all libraries except for glibc and libstdc++ statically.
ARG STATIC_BUILD_WITH_DYNAMIC_STDLIB=OFF
# Whether to install dependencies required to run PTF-ebpf tests
ARG INSTALL_PTF_EBPF_DEPENDENCIES=OFF
# Whether to build the P4Tools back end and platform.
ARG ENABLE_TEST_TOOLS=ON
# Whether to treat warnings as errors.
ARG ENABLE_WERROR=ON
# Compile with Clang compiler
ARG COMPILE_WITH_CLANG=OFF
# Compile with sanitizers (UBSan, ASan)
ARG ENABLE_SANITIZERS=OFF
# Only execute the steps necessary to successfully run CMake.
ARG CMAKE_ONLY=OFF
# Build with -ftrivial-auto-var-init=pattern to catch more bugs caused by
# uninitialized variables.
ARG BUILD_AUTO_VAR_INIT_PATTERN=OFF

# Configuration of ASAN and UBSAN sanitizers:
# - Print symbolized stack trace for each error report.
# - Disable leaks detector as p4c uses GC.
ENV UBSAN_OPTIONS=print_stacktrace=1
ENV ASAN_OPTIONS=print_stacktrace=1:detect_leaks=0

# Delegate the build to tools/ci-build.
COPY --from=reducebase /home/p4cherry /home/p4cherry
RUN /home/p4cherry/p4c/tools/ci-build.sh
# Set the workdir after building p4c.
WORKDIR /home/p4cherry
