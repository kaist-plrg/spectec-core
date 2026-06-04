# Materials for MechSpec@PLDI26

Materials for part 3 of the MechSpec tutorial session, at PLDI'26. Contains a small typed imperative language ("Typed Imp"), specified in SpecTecX. Follow the tutorial and copy-paste commands from this file when necessary.

The binary (`spectecx`) is obtained separately -- see [Getting the tool](#getting-the-tool). The commands below assume it is on your `PATH`; otherwise prefix with `./`.

## Layout

```
.
|-- impty.spectec        the language spec: base + first-class-function stubs
|-- spectecx.config      project-local CLI defaults
|-- Makefile             command helpers: tests + prose (`make help`)
|-- Dockerfile           build-from-source fallback
|-- tests/
|   |-- base/             base-language programs (run as-is)
|   `-- functions/        programs using functions (run once the stubs are filled in)
`-- documentation/
    |-- impty.adoc        prose document source (AsciiDoc + splice directives)
    `-- docinfo.html      stylesheet for the rendered output
```

The commands below read `spectecx.config` from the current directory for their `--spec` and `--batch-dir` defaults, so run them from this directory.

## Getting the tool

```sh
# 1. Prebuilt binary: download from the project's GitHub Releases, then
chmod +x spectecx && sudo mv spectecx /usr/local/bin/    # or keep it here and use ./spectecx

# 2. Build from source (opam, OCaml >= 5.1, GMP headers):
git clone https://github.com/kaist-plrg/spectecx.git && cd spectecx
opam switch create spectecx 5.1.0
opam install -y --switch=spectecx --deps-only ./spectec
make exe                                                 # produces ./spectecx

# 3. Docker (no host toolchain; also includes asciidoctor):
docker build -t spectecx-tutorial .                      # run from this directory
docker run --rm -v "$PWD":/work spectecx-tutorial \
  spectecx impty batch --batch-dir tests/base
```

## 1. Typed Imp: executable inference rules

`impty.spectec` defines the language as inference rules -- syntax, typing, and evaluation. The rules are executable: the tool runs a program by building a derivation from them.

```sh
# typecheck, then run hello.imp
./spectecx impty typecheck -p tests/base/hello.imp
./spectecx impty eval      -p tests/base/hello.imp

# show the derivation tree the run was built from, in spec syntax
./spectecx impty eval -p tests/base/hello.imp --tree.level conclusion
```

## 2. Adding first-class functions (test-driven)

`impty.spectec` has the syntax for functions but leaves four rules as `-- TODO` stubs. The base programs already pass; the function programs fail until the stubs are filled in. Fill in a rule, re-run, repeat.

```sh
# base programs pass; the full suite has the 4 function programs failing
make test-base
make test

# debug one program at a time
./spectecx impty typecheck -p tests/functions/closure.imp
./spectecx impty eval      -p tests/functions/closure.imp
```

## 3. Testing: coverage and property-based testing

```sh
# coverage: which rules the suite exercises (summary lists the uncovered ones)
./spectecx impty batch --branch-coverage.level summary

# property-based testing of type safety
./spectecx impty quickcheck
./spectecx impty quickcheck --generalize
./spectecx impty quickcheck --num-tests 400 --branch-coverage.level summary
```

## 4. Documentation: generated prose

The prose document splices straight from `impty.spectec`. Needs [asciidoctor](https://asciidoctor.org/) (`gem install asciidoctor asciidoctor-pdf`), or use the Docker image above.

```sh
make splice-html    # -> documentation/impty.html
make splice-pdf     # -> documentation/impty.pdf
```

Then open `documentation/impty.html` (or `documentation/impty.pdf`).
