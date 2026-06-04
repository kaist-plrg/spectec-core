# Typed Imp tutorial - command helpers.
#
# Wraps the test suite and the prose splice pipeline; everything else is a
# plain `spectecx impty ...` command (see README.md).

SPECTECX ?= ./spectecx
SPEC      = impty.spectec

DOC_DIR   = documentation
DOC_BUILD = $(DOC_DIR)/_build

.PHONY: test test-base splice splice-html splice-pdf clean help

help:
	@echo "Tutorial helpers (see README.md for the full 'spectecx impty ...' set):"
	@echo "  make test          run the full test suite (tests/)"
	@echo "  make test-base     run the base-language tests only (tests/base/)"
	@echo "  make splice-html   render the prose document to documentation/impty.html"
	@echo "  make splice-pdf    render the prose document as documentation/impty.pdf"
	@echo "  make clean         remove the build dir and rendered output"
	@echo "Override the tool with SPECTECX=... (default: $(SPECTECX))."
	@echo "Early finishers: an optional recursion exercise lives in recursion/ (see recursion/README.md)."

test:
	$(SPECTECX) impty batch

test-base:
	$(SPECTECX) impty batch --batch-dir tests/base

# splice-html needs asciidoctor; splice-pdf needs asciidoctor-pdf.

splice:
	mkdir -p $(DOC_BUILD)
	$(SPECTECX) splice -i $(DOC_DIR) -o $(DOC_BUILD) \
	  --missing $(DOC_BUILD)/splice.missing $(SPEC)

splice-html: splice
	asciidoctor -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.html $(DOC_BUILD)/impty.adoc

splice-pdf: splice
	asciidoctor-pdf -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.pdf $(DOC_BUILD)/impty.adoc

clean:
	rm -rf $(DOC_BUILD) $(DOC_DIR)/impty.html $(DOC_DIR)/impty.pdf
