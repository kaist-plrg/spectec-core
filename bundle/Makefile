# Typed Imp tutorial - command helpers.
#
# Wraps the test suite and the prose document pipeline; everything else is a
# plain `spectecx impty ...` command (see README.md).

SPECTECX ?= ./spectecx
SPEC      = impty.spectec

DOC_DIR   = documentation
DOC_BUILD = $(DOC_DIR)/_build

.PHONY: check test test-base doc doc-html doc-pdf clean help

help:
	@echo "Tutorial helpers (see README.md for the full 'spectecx impty ...' set):"
	@echo "  make check         type-check the spec (errors and warnings, no IL)"
	@echo "  make test          run the full test suite (tests/)"
	@echo "  make test-base     run the base-language tests only (tests/base/)"
	@echo "  make doc-html      render the prose document to documentation/impty.html"
	@echo "  make doc-pdf       render the prose document as documentation/impty.pdf"
	@echo "  make clean         remove the build dir and rendered output"
	@echo "Override the tool with SPECTECX=... (default: $(SPECTECX))."
	@echo "Early finishers: an optional recursion exercise lives in recursion/ (see recursion/README.md)."

check:
	$(SPECTECX) elab --check $(SPEC)

test:
	$(SPECTECX) impty batch

test-base:
	$(SPECTECX) impty batch --batch-dir tests/base

# doc-html needs asciidoctor; doc-pdf needs asciidoctor-pdf.

doc:
	mkdir -p $(DOC_BUILD)
	$(SPECTECX) splice -i $(DOC_DIR) -o $(DOC_BUILD) \
	  --missing $(DOC_BUILD)/splice.missing $(SPEC)

doc-html: doc
	asciidoctor -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.html $(DOC_BUILD)/impty.adoc

doc-pdf: doc
	asciidoctor-pdf -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.pdf $(DOC_BUILD)/impty.adoc

clean:
	rm -rf $(DOC_BUILD) $(DOC_DIR)/impty.html $(DOC_DIR)/impty.pdf
