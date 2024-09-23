MAIN = p4cherry
TEST = p4cherry-test

# Compile

.PHONY: build

EXEMAIN = _build/default/bin/main.exe
EXETEST = _build/default/bin/test.exe

build:
	rm -f ./$(MAIN)
	dune build && echo
	ln -f $(EXEMAIN) ./$(MAIN)
	ln -f $(EXETEST) ./$(TEST)

# Format

.PHONY: fmt

fmt:
	dune build @fmt --auto-promote

# Tests

.PHONY: test-parser test-typecheck test

test-parser: build
	./$(TEST) parse -i test/arch test/program > status/parser.log 2> status/parser.err

test-typecheck: build
	./$(TEST) typecheck -i test/arch test/program > status/typecheck.log 2> status/typecheck.err

test: test-parser test-typecheck

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME) ./$(TEST)
	dune clean
