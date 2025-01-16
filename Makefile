MAIN = p4cherry
TEST = p4cherry-test
SPEC = watsup

# Compile

.PHONY: build build-p4 build-spectec

EXEMAIN = p4/_build/default/bin/main.exe
EXETEST = p4/_build/default/bin/test.exe
EXESPEC = spectec/spectec/_build/default/src/exe-watsup/main.exe

build: build-p4 build-spectec

build-p4:
	rm -f ./$(MAIN) ./$(TEST)
	opam switch 4.14.0
	cd p4 && opam exec dune build && echo
	ln -f $(EXEMAIN) ./$(MAIN)
	ln -f $(EXETEST) ./$(TEST)

build-p4-release:
	rm -f ./$(MAIN) ./$(TEST)
	opam switch 4.14.0
	cd p4 && opam exec -- dune build --profile release && echo
	ln -f $(EXEMAIN) ./$(MAIN)
	ln -f $(EXETEST) ./$(TEST)

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

.PHONY: test test-parser test-typecheck

test: test-parser test-typecheck-pos test-typecheck-neg test-instantiate test-run-v1model

test-parser: build-p4-release
	./$(TEST) parse -i p4/test/arch p4/test/p4c/program/well-typed > p4/status/p4c/parser.log 2> p4/status/p4c/parser.err
	./$(TEST) parse -i p4/test/arch p4/test/petr4/program > p4/status/petr4/parser.log 2> p4/status/petr4/parser.err

test-typecheck-pos: build-p4-release
	./$(TEST) typecheck -i p4/test/arch -p p4/test/p4c/program/well-typed > p4/status/p4c/typecheck-pos.log 2> p4/status/p4c/typecheck-pos.err
	./$(TEST) typecheck -i p4/test/arch -p p4/test/p4c/program/well-typed-excluded > p4/status/p4c/typecheck-pos-excluded.log 2> p4/status/p4c/typecheck-pos-excluded.err
	./$(TEST) typecheck -i p4/test/arch -p p4/test/petr4/program/well-typed > p4/status/petr4/typecheck-pos.log 2> p4/status/petr4/typecheck-pos.err
	./$(TEST) typecheck -i p4/test/arch -p p4/test/petr4/program/well-typed-excluded > p4/status/petr4/typecheck-pos-excluded.log 2> p4/status/petr4/typecheck-pos-excluded.err

test-typecheck-neg: build-p4-release
	./$(TEST) typecheck -i p4/test/arch -n p4/test/p4c/program/ill-typed > p4/status/p4c/typecheck-neg.log 2> p4/status/p4c/typecheck-neg.err
	./$(TEST) typecheck -i p4/test/arch -n p4/test/p4c/program/ill-typed-excluded > p4/status/p4c/typecheck-neg-excluded.log 2> p4/status/p4c/typecheck-neg-excluded.err
	./$(TEST) typecheck -i p4/test/arch -n p4/test/petr4/program/ill-typed > p4/status/petr4/typecheck-neg.log 2> p4/status/petr4/typecheck-neg.err

test-instantiate: build-p4-release
	./$(TEST) instantiate -i p4/test/arch p4/test/p4c/program/well-typed > p4/status/p4c/instantiate.log 2> p4/status/p4c/instantiate.err
	./$(TEST) instantiate -i p4/test/arch p4/test/petr4/program/well-typed > p4/status/petr4/instantiate.log 2> p4/status/petr4/instantiate.err

test-run-v1model: build-p4-release
	./$(TEST) run -a v1model -i p4/test/arch -p p4/test/p4c/stf/v1model-patch p4/test/p4c/program/well-typed p4/test/p4c/stf/v1model > p4/status/p4c/run-v1model.log 2> p4/status/p4c/run-v1model.err
	./$(TEST) run -a v1model -i p4/test/arch -p p4/test/petr4/stf-patch p4/test/petr4/program/well-typed p4/test/petr4/stf > p4/status/petr4/run-v1model.log 2> p4/status/petr4/run-v1model.err

# Cleanup

.PHONY: clean

clean:
	rm -f ./$(MAIN) ./$(TEST) ./$(SPEC)
	cd p4 && dune clean
	cd spectec/spectec && dune clean
