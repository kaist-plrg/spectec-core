# Typed Imp tutorial - command helpers.
#
# Wraps the test suite and the prose document pipeline; everything else is a
# plain `spectecx impty ...` command (see README.md).

SPECTECX ?= ./spectecx
SPEC      = impty.spectec
QC_DIR    = counterexamples

SKEL_DIR  = skeleton
DOC_DIR   = documentation

.PHONY: check test test-base quickcheck verify doc doc-html doc-pdf clean help

help:
	@echo "Tutorial helpers (see README.md for the full 'spectecx impty ...' set):"
	@echo "  make check         type-check the spec (errors and warnings, no IL)"
	@echo "  make test-base     run the base-language tests only (tests/base/)"
	@echo "  make test          run the full test suite (tests/)"
	@echo "  make quickcheck    property-test type safety (saves to $(QC_DIR)/)"
	@echo "  make verify        run the test suite, then quickcheck (the full check)"
	@echo "  make doc           splice the prose to documentation/impty.adoc"
	@echo "  make doc-html      render the prose document to documentation/impty.html (requires asciidoctor)"
	@echo "  make doc-pdf       render the prose document as documentation/impty.pdf (requires asciidoctor-pdf)"
	@echo "  make clean         remove the generated prose and rendered output"
	@echo "Override the tool with SPECTECX=... (default: $(SPECTECX))."
	@echo "Early finishers: an optional recursion exercise lives in recursion/ (see recursion/README.md)."

check:
	$(SPECTECX) elab --check $(SPEC)

test:
	$(SPECTECX) impty batch

test-base:
	$(SPECTECX) impty batch --batch-dir tests/base

quickcheck:
	$(SPECTECX) impty quickcheck --save-dir $(QC_DIR)

# The full check, in the order the README walks through: the provided tests
# first, then property-based testing.
verify: test quickcheck

# doc-html needs asciidoctor; doc-pdf needs asciidoctor-pdf.

doc:
	mkdir -p $(DOC_DIR)
	$(SPECTECX) splice -i $(SKEL_DIR) -o $(DOC_DIR) $(SPEC)
	@echo "wrote $(DOC_DIR)/impty.adoc"

doc-html: doc
	asciidoctor -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.html $(DOC_DIR)/impty.adoc

doc-pdf: doc
	asciidoctor-pdf -q \
	  -a docinfo=shared -a docinfodir=$(abspath $(DOC_DIR)) \
	  -o $(DOC_DIR)/impty.pdf $(DOC_DIR)/impty.adoc

clean:
	rm -f $(DOC_DIR)/impty.adoc $(DOC_DIR)/impty.html $(DOC_DIR)/impty.pdf
	rm -rf $(QC_DIR)
