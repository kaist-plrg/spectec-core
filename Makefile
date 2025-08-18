SPEC = p4spectec

# Compile

.PHONY: build build-spec

EXEMAIN = p4/_build/default/bin/main.exe
EXESPEC = p4spec/_build/default/bin/main.exe

build: build-spec

build-spec:
	rm -f ./$(SPEC)
	rm -f ./p4spec/lib/parsing/parser.ml ./p4spec/lib/parsing/parser.mli
	opam switch 4.14.0
	cd p4spec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(SPEC)

# Format

.PHONY: fmt

fmt:
	opam switch 4.14.0
	cd p4spec && opam exec dune fmt

# Tests

.PHONY: test-spec test-spec-inst promote-spec

test-spec:
	echo "#### Running (dune runtest)"
	opam switch 4.14.0
	cd p4spec && opam exec -- dune runtest test --profile=release && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote-spec\` to accept changes in test expectations. && false)

test-spec-inst:
	echo "#### Running inst-il tests"
	opam switch 4.14.0
	cd p4spec && opam exec -- dune runtest test-inst --profile=release && echo OK || (echo "####>" Failure running inst-il tests. && echo "####>" Run \`make promote-spec\` to accept changes in test expectations. && false)

promote-spec:
	opam switch 4.14.0
	cd p4spec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(SPEC)
	cd p4spec && dune clean
