NAME = p4cherry

# Compile

.PHONY: build

EXE = _build/default/bin/main.exe

build:
	rm -f ./$(NAME)
	dune build && echo
	ln -f $(EXE) ./$(NAME)

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(NAME)
	dune clean
