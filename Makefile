MAIN = p4cherry
WATSUP = watsup
SPEC = p4spectec

# Compile

.PHONY: build build-p4 build-spectec

EXEMAIN = p4/_build/default/bin/main.exe
EXEWATSUP = spectec/spectec/_build/default/src/exe-watsup/main.exe
EXESPEC = p4spec/_build/default/bin/main.exe

build: build-p4 build-watsup

build-p4:
	rm -f ./$(MAIN)
	opam switch 4.14.0
	cd p4 && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXEMAIN) ./$(MAIN)

build-p4-release:
	rm -f ./$(MAIN)
	opam switch 4.14.0
	cd p4 && opam exec -- dune build --profile release bin/main.exe && echo
	ln -f $(EXEMAIN) ./$(MAIN)

build-watsup:
	rm -f ./$(WATSUP)
	opam switch 5.0.0
	cd spectec/spectec && opam exec make
	ln -f $(EXEWATSUP) ./$(WATSUP)

build-spec:
	rm -f ./$(SPEC)
	opam switch 4.14.0
	cd p4spec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)

# Spec

spec: build-watsup
	echo "This will be broken until p4spec catches up with watsup ..."
	./$(SPEC) --latex spec/*.watsup	> spec/spec-gen.include
	cd spec && pdflatex spec.tex
	echo "Spec generation completed: spec/spec.pdf"

# Format

.PHONY: fmt

fmt:
	opam switch 4.14.0
	cd p4 && opam exec dune fmt
	cd p4spec && opam exec dune fmt

# Tests

.PHONY: test-p4 promote-p4 coverage-p4 test-spec

test-p4:
	echo "#### Running (dune runtest)"
	opam switch 4.14.0
	cd p4 && dune clean && opam exec -- dune runtest && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote-p4\` to accept changes in test expectations. && false)

promote-p4:
	opam switch 4.14.0
	cd p4 && opam exec -- dune promote

coverage-p4:
	echo "#### Running (dune runtest --instrument-with bisect_ppx --force)"
	opam switch 4.14.0
	cd p4 && dune clean && find . -name '*.coverage' | xargs rm -f && opam exec -- dune runtest --instrument-with bisect_ppx --force
	cd p4 && bisect-ppx-report html && bisect-ppx-report summary

test-spec:
	echo "#### Running (dune runtest)"
	opam switch 4.14.0
	cd p4spec && dune clean && opam exec -- dune runtest --profile=release && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote-spec\` to accept changes in test expectations. && false)

promote-spec:
	opam switch 4.14.0
	cd p4spec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(MAIN) ./$(WATSUP) ./$(SPEC)
	cd p4 && dune clean
	cd spectec/spectec && dune clean
	cd p4spec && dune clean
