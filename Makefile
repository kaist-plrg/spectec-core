MAIN = p4cherry
TEST = p4cherry-test

# Compile

.PHONY: build

EXEMAIN = _build/default/bin/main.exe
EXETEST = _build/default/bin/test.exe

build:
	rm -f ./$(MAIN) ./$(TEST)
	dune build && echo
	ln -f $(EXEMAIN) ./$(MAIN)
	ln -f $(EXETEST) ./$(TEST)

# Format

.PHONY: fmt

fmt:
	dune build @fmt --auto-promote

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME) ./$(TEST)
	dune clean
