NAME = spectecx

SWITCH ?= spectecx

OPAM_EXEC = opam exec --switch=$(SWITCH) --
DUNE = cd spectec && $(OPAM_EXEC) dune

# Compile & Format

.PHONY: exe lsp check fmt fmt-check promote clean

EXESPEC = spectec/_build/default/bin/main.exe
EXELSP = spectec/_build/default/bin/lsp_main.exe

exe:
	rm -f ./$(NAME)
	$(DUNE) build bin/main.exe
	@echo
	ln -f $(EXESPEC) ./$(NAME)

lsp:
	rm -f ./$(NAME)-lsp
	$(DUNE) build bin/lsp_main.exe
	@echo
	ln -f $(EXELSP) ./$(NAME)-lsp

check:
	$(DUNE) build @check

fmt:
	$(DUNE) fmt

fmt-check:
	$(DUNE) build @fmt

promote:
	$(DUNE) promote

clean:
	rm -f ./$(NAME) ./$(NAME)-lsp
	$(DUNE) clean

# Splice and render. `splice-html` requires `asciidoctor`; `splice-pdf`
# requires `asciidoctor-pdf`.

SPLICE_INPUT = spectec/examples/splice
SPLICE_BUILD = spectec/examples/splice/_build
IMPTY_SPEC = spectec/specs/impty/base/spec.spectec

.PHONY: splice splice-html splice-pdf splice-clean

splice: exe
	mkdir -p $(SPLICE_BUILD)
	./$(NAME) splice -i $(SPLICE_INPUT) -o $(SPLICE_BUILD) \
	  --missing $(SPLICE_BUILD)/splice.missing $(IMPTY_SPEC)

splice-html: splice
	asciidoctor -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(SPLICE_INPUT)) \
	  -o $(SPLICE_BUILD)/impty.html $(SPLICE_BUILD)/impty.adoc

splice-pdf: splice
	asciidoctor-pdf -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(SPLICE_INPUT)) \
	  -o $(SPLICE_BUILD)/impty.pdf $(SPLICE_BUILD)/impty.adoc

splice-clean:
	rm -rf $(SPLICE_BUILD)

# VS Code extension: package editors/vscode into a sideloadable .vsix
# (install with `code --install-extension spectecx.vsix`).

.PHONY: vsix

vsix:
	cd editors/vscode && npx -y @vscode/vsce package -o $(NAME).vsix
	@echo "#### extension written to editors/vscode/$(NAME).vsix"

# Tests
#
# Individual tests (run against the new p4 spec by default):
#   make test-elab       - Elaboration test (both p4 and p4-old)
#   make test-struct     - Structuring test (both p4 and p4-old)
#   make test-annotate   - Annotate/prose render test (impty x3 + p4-old)
#   make test-roundtrip-il - EL<->IL premise roundtrip test (impty base + closure, p4)
#   make test-roundtrip-el - EL pretty-printer roundtrip test (mini-spec, p4-old, p4, impty)
#   make test-parsegen   - Grammar-driven parser differential test (impty expressions + programs)
#   make test-il-pos     - IL interpreter positive tests (slow)
#   make test-il-neg     - IL interpreter negative tests
#   make test-sl-pos     - SL interpreter positive tests (slow)
#   make test-sl-neg     - SL interpreter negative tests
#
# p4-old interpreter tests:
#   make test-il-pos-old / test-il-neg-old / test-sl-pos-old / test-sl-neg-old
#
# Per-case interpreter negative corpus (impty IL, one cram test per case):
#   make test-interp-neg - Per-case impty IL negative tests
#
# CLI snapshot tests (impty CLI + instrumentation, cram against hello.imp):
#   make test-cli        - impty CLI command and instrumentation snapshots
#   make test-lsp        - LSP diagnostics snapshot (Check.run -> LSP JSON)
#
# Grouped tests:
#   make test-quick      - Fast tests (elab + elab-neg + interp-neg + cli + struct + annotate + roundtrip-il + roundtrip-el + impty + parsegen)
#   make test-il         - IL tests for new p4 (pos + neg)
#   make test-sl         - SL tests for new p4 (pos + neg)
#   make test-il-old     - IL tests for p4-old (pos + neg)
#   make test-sl-old     - SL tests for p4-old (pos + neg)
#   make test-old        - All p4-old interpreter tests
#
# impty interpreter tests (per-variant: base, closure):
#   make test-impty-<v>-il-pos / -il-neg / -sl-pos / -sl-neg
#   make test-impty-<v>-il / -sl                     - per-variant pos+neg
#   make test-impty-<v>                              - per-variant il+sl
#   make test-impty                                  - all impty tests
#
#   make test            - quick + new p4 il/sl

.PHONY: test test-quick test-elab test-elab-neg test-interp-neg test-cli test-lsp test-struct test-annotate test-roundtrip-il test-roundtrip-el test-parsegen
.PHONY: test-il test-il-pos test-il-neg
.PHONY: test-sl test-sl-pos test-sl-neg
.PHONY: test-old test-il-old test-il-pos-old test-il-neg-old
.PHONY: test-sl-old test-sl-pos-old test-sl-neg-old
.PHONY: test-impty test-impty-base test-impty-closure
.PHONY: test-impty-base-il test-impty-base-sl
.PHONY: test-impty-closure-il test-impty-closure-sl
.PHONY: test-impty-base-il-pos test-impty-base-il-neg
.PHONY: test-impty-base-sl-pos test-impty-base-sl-neg
.PHONY: test-impty-closure-il-pos test-impty-closure-il-neg
.PHONY: test-impty-closure-sl-pos test-impty-closure-sl-neg
.PHONY: promote

test-elab:
	@echo "#### Running elaboration test"
	@$(DUNE) build @test/elab/runtest --profile=release && echo OK

test-roundtrip-el:
	@echo "#### Running EL pretty-printer roundtrip test"
	@$(DUNE) build @test/roundtrip/el/runtest --profile=release && echo OK

test-elab-neg:
	@echo "#### Running elaboration negative tests"
	@$(DUNE) build @test/elab/neg/runtest --profile=release && echo OK

test-interp-neg:
	@echo "#### Running interpreter negative tests (per-case impty IL corpus)"
	@$(DUNE) build @test/interp/neg/runtest --profile=release && echo OK

test-cli:
	@echo "#### Running CLI snapshot tests (impty CLI + instrumentation)"
	@$(DUNE) build @test/cli/runtest --profile=release && echo OK

test-lsp:
	@echo "#### Running LSP diagnostics test"
	@$(DUNE) build @test/lsp/runtest --profile=release && echo OK

test-struct:
	@echo "#### Running structuring test"
	@$(DUNE) build @test/struct/runtest --profile=release && echo OK

test-annotate:
	@echo "#### Running annotate test"
	@$(DUNE) build @test/annotate/runtest --profile=release && echo OK

test-roundtrip-il:
	@echo "#### Running EL<->IL premise roundtrip test"
	@$(DUNE) build @test/roundtrip/il/runtest --profile=release && echo OK

test-parsegen:
	@echo "#### Running grammar-driven parser differential test"
	@$(DUNE) build @test/parsegen/runtest --profile=release && echo OK

# $(1): target prefix (p4 / p4-old)
# $(2): il / sl
# $(3): pos / neg
define run_interp_test
	@echo "#### Running $(2) interpreter $(3) tests ($(1))"
	@$(DUNE) build @test/interp/$(1)-$(2)-$(3) --profile=release
	@cat spectec/_build/default/test/interp/$(1)-$(2)-$(3).err >&2
	@echo OK
endef

test-il-pos:
	$(call run_interp_test,p4,il,pos)

test-il-neg:
	$(call run_interp_test,p4,il,neg)

test-sl-pos:
	$(call run_interp_test,p4,sl,pos)

test-sl-neg:
	$(call run_interp_test,p4,sl,neg)

test-il-pos-old:
	$(call run_interp_test,p4-old,il,pos)

test-il-neg-old:
	$(call run_interp_test,p4-old,il,neg)

test-sl-pos-old:
	$(call run_interp_test,p4-old,sl,pos)

test-sl-neg-old:
	$(call run_interp_test,p4-old,sl,neg)

test-quick: test-elab test-elab-neg test-interp-neg test-cli test-lsp test-struct test-annotate test-roundtrip-il test-roundtrip-el test-impty test-parsegen
	@echo "#### Quick tests passed"

test-il: test-il-pos test-il-neg
	@echo "#### IL tests passed"

test-sl: test-sl-pos test-sl-neg
	@echo "#### SL tests passed"

test-il-old: test-il-pos-old test-il-neg-old
	@echo "#### IL (p4-old) tests passed"

test-sl-old: test-sl-pos-old test-sl-neg-old
	@echo "#### SL (p4-old) tests passed"

test-old: test-il-old test-sl-old
	@echo "#### p4-old interpreter tests passed"

test-impty-base-il-pos:
	$(call run_interp_test,impty-base,il,pos)

test-impty-base-il-neg:
	$(call run_interp_test,impty-base,il,neg)

test-impty-base-sl-pos:
	$(call run_interp_test,impty-base,sl,pos)

test-impty-base-sl-neg:
	$(call run_interp_test,impty-base,sl,neg)

test-impty-closure-il-pos:
	$(call run_interp_test,impty-closure,il,pos)

test-impty-closure-il-neg:
	$(call run_interp_test,impty-closure,il,neg)

test-impty-closure-sl-pos:
	$(call run_interp_test,impty-closure,sl,pos)

test-impty-closure-sl-neg:
	$(call run_interp_test,impty-closure,sl,neg)

test-impty-base-il: test-impty-base-il-pos test-impty-base-il-neg
	@echo "#### IL (impty-base) tests passed"

test-impty-base-sl: test-impty-base-sl-pos test-impty-base-sl-neg
	@echo "#### SL (impty-base) tests passed"

test-impty-closure-il: test-impty-closure-il-pos test-impty-closure-il-neg
	@echo "#### IL (impty-closure) tests passed"

test-impty-closure-sl: test-impty-closure-sl-pos test-impty-closure-sl-neg
	@echo "#### SL (impty-closure) tests passed"

test-impty-base: test-impty-base-il test-impty-base-sl
	@echo "#### impty-base interpreter tests passed"

test-impty-closure: test-impty-closure-il test-impty-closure-sl
	@echo "#### impty-closure interpreter tests passed"

test-impty: test-impty-base test-impty-closure
	@echo "#### impty interpreter tests passed"

test: test-quick test-il test-sl
	@echo "#### All quick tests + p4 + impty interpreter tests passed"
