# Self-contained tutorial image: spectecx, the materials, an editor, and
# asciidoctor. A prebuilt image is published (docker pull
# kaistplrg/spectecx:tutorial); to build it yourself:
#   docker build -t spectecx-tutorial . && docker run -it spectecx-tutorial
# Override the revision with --build-arg SPECTECX_REF=<tag-or-branch>.

FROM ocaml/opam:debian-12-ocaml-5.1

# opam deps need GMP; asciidoctor needs Ruby; vim/nano to edit the spec.
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

# Symlink the binary as ./spectecx so README commands run verbatim from /work.
COPY --chown=opam . /work
RUN ln -sf /usr/local/bin/spectecx /work/spectecx

# Preconfigure Vim with the bundled highlighter and a dark theme.
RUN mkdir -p /home/opam/.vim \
 && cp -r /work/editors/vim/syntax /work/editors/vim/ftdetect /home/opam/.vim/ \
 && printf 'syntax on\nfiletype plugin on\nset background=dark\nsilent! colorscheme habamax\n' \
      > /home/opam/.vimrc

WORKDIR /work
ENTRYPOINT []
CMD ["bash"]
