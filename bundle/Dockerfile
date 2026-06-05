# Typed Imp tutorial - a self-contained image: `spectecx`, the tutorial
# materials, an editor, and asciidoctor, so participants need no host
# toolchain.
#
# A prebuilt image is published, so most people can skip this build:
#
#   docker pull kunjeong/spectecx:tutorial
#   docker run -it kunjeong/spectecx:tutorial
#
# To build the image yourself instead, work entirely inside the container:
#
#   docker build -t spectecx-tutorial .
#   docker run -it spectecx-tutorial          # shell in /work; edit and run here
#
# Inside, every README command runs as written (./spectecx ..., make test, ...).
# To keep edits across sessions, drop --rm and reuse a named container, or
# mount your spec: -v "$PWD/impty.spectec":/work/impty.spectec
#
# Override the revision with --build-arg SPECTECX_REF=<tag-or-branch>.

FROM ocaml/opam:debian-12-ocaml-5.1

# opam deps need GMP (bignum); asciidoctor needs Ruby; vim/nano to edit the spec.
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
      libgmp-dev pkg-config m4 \
      ruby ruby-dev build-essential \
      vim nano \
 && gem install --no-document asciidoctor asciidoctor-pdf \
 && rm -rf /var/lib/apt/lists/*
USER opam

ARG SPECTECX_REPO=https://github.com/kaist-plrg/spectecx.git
ARG SPECTECX_REF=main

RUN git clone --depth 1 --branch "$SPECTECX_REF" "$SPECTECX_REPO" /home/opam/spectecx
WORKDIR /home/opam/spectecx/spectec

RUN opam install -y --deps-only . \
 && opam exec -- dune build bin/main.exe \
 && sudo install _build/default/bin/main.exe /usr/local/bin/spectecx

# Bake the tutorial materials in, and expose the binary as ./spectecx so the
# README commands run unchanged from /work.
COPY --chown=opam . /work
RUN ln -sf /usr/local/bin/spectecx /work/spectecx

WORKDIR /work
ENTRYPOINT []
CMD ["bash"]
