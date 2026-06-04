# Typed Imp -- recursion exercise (optional, for early finishers).
#
# Self-contained: run these from this directory. `spectecx` is a symlink to the
# binary in the parent bundle directory, and spectecx.config supplies the
# --spec and --batch-dir defaults.

SPECTECX ?= ./spectecx

.PHONY: test-rec test-base help

help:
	@echo "Recursion exercise helpers (see README.md):"
	@echo "  make test-rec    run the whole suite (tests/: base + functions + recursion)"
	@echo "  make test-base   run the base-language tests only (tests/base/)"
	@echo "Override the tool with SPECTECX=... (default: $(SPECTECX))."

test-rec:
	$(SPECTECX) impty batch

test-base:
	$(SPECTECX) impty batch --batch-dir tests/base
