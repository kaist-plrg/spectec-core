MAIN = p4cherry
SPEC = watsup

# Compile

.PHONY: build build-p4 build-spectec

EXEMAIN = p4/_build/default/bin/main.exe
EXETEST = p4/_build/default/bin/test.exe
EXESPEC = spectec/spectec/_build/default/src/exe-watsup/main.exe

build: build-p4 build-spectec

build-p4:
	rm -f ./$(MAIN)
	opam switch 4.14.0
	cd p4 && opam exec dune build && echo
	ln -f $(EXEMAIN) ./$(MAIN)

build-p4-release:
	rm -f ./$(MAIN)
	opam switch 4.14.0
	cd p4 && opam exec -- dune build --profile release && echo
	ln -f $(EXEMAIN) ./$(MAIN)

build-spectec:
	rm -f ./$(SPEC)
	opam switch 5.0.0
	cd spectec/spectec && opam exec make
	ln -f $(EXESPEC) ./$(SPEC)

# Spec

spec: build-spectec
	./$(SPEC) --latex spec/*.watsup	> spec/spec-gen.include
	cd spec && pdflatex spec.tex
	echo "Spec generation completed: spec/spec.pdf"

# Format

.PHONY: fmt

fmt:
	opam switch 4.14.0
	cd p4 && opam exec dune fmt

# Tests

.PHONY: test promote coverage 

test:
	echo "#### Running (dune runtest)"
	opam switch 4.14.0
	cd p4 && dune clean && opam exec -- dune runtest && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote\` to accept changes in test expectations. && false)

promote:
	opam switch 4.14.0
	cd p4 && opam exec -- dune promote

coverage:
	echo "#### Running (dune runtest --instrument-with bisect_ppx --force)"
	opam switch 4.14.0
	cd p4 && dune clean && find . -name '*.coverage' | xargs rm -f && opam exec -- dune runtest --instrument-with bisect_ppx --force
	cd p4 && bisect-ppx-report html && bisect-ppx-report summary

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(MAIN) ./$(SPEC)
	cd p4 && dune clean
	cd spectec/spectec && dune clean
