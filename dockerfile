# --------------------------------------
# Stage 1: System dependencies
# --------------------------------------
FROM ubuntu:20.04 AS base

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Seoul

RUN apt-get update && \
    apt-get install -y git make curl && \
    apt-get clean

WORKDIR /home

# --------------------------------------
# Stage 2: Clone repo
# --------------------------------------
FROM base AS source

RUN git clone https://github.com/kaist-plrg/p4cherry.git && \
    cd p4cherry && \
    git checkout p4spec-sl-mod-il && \
    git submodule update --init

WORKDIR /home/p4cherry

# ---------------------------------------
# Stage 3: Installations - p4cherry/p4spec
# ---------------------------------------
FROM source AS opambase

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Seoul

RUN apt-get update && \
    apt-get install -y opam libgmp-dev pkg-config && \
    apt-get clean

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
    apt-get clean

COPY patches/creduce /usr/bin/creduce
RUN chmod +x /usr/bin/creduce

ENV P4CHERRY_PATH=/home/p4cherry
