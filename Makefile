NAME = spectec-core

# Compile

.PHONY: build build-spec

EXESPEC = spectec/_build/default/bin/main.exe

build: build-spec

build-spec:
	rm -f ./$(NAME)
	opam switch 5.1.0
	cd spectec && opam exec -- dune build bin/main.exe && echo
	ln -f $(EXESPEC) ./$(NAME)

# Format

.PHONY: fmt

fmt:
	opam switch 5.1.0
	cd spectec && opam exec dune fmt

# Tests

.PHONY: test-spec promote-spec

test-spec:
	echo "#### Running (dune runtest)"
	opam switch 5.1.0
	cd spectec && opam exec -- dune runtest --profile=release && echo OK || (echo "####>" Failure running dune test. && echo "####>" Run \`make promote-spec\` to accept changes in test expectations. && false)

promote-spec:
	opam switch 5.1.0
	cd spectec && opam exec -- dune promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME)
	cd spectec && dune clean
