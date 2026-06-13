# Typed Imp -- recursion exercise (optional, for early finishers).
#
# Self-contained: run these from this directory. `spectecx` is a symlink to the
# binary in the parent bundle directory, and spectecx.config supplies the
# --spec and --batch-dir defaults.

SPECTECX ?= ./spectecx
SPEC      = recursion.spectec

.PHONY: check import test test-base help

help:
	@echo "Recursion exercise helpers (see README.md):"
	@echo "  make import      bring your completed function rules over from ../impty.spectec"
	@echo "  make check       type-check the spec (errors and warnings, no IL)"
	@echo "  make test        run the whole suite (tests/: base + functions + recursion)"
	@echo "  make test-base   run base + functions (everything but the recursion programs)"
	@echo "Override the tool with SPECTECX=... (default: $(SPECTECX))."

# Copy the completed (functions) rules over from the main exercise. Refuses
# until ../impty.spectec is finished (see import-functions.sh).
import:
	@sh import-functions.sh

check:
	$(SPECTECX) elab --check $(SPEC)

test:
	$(SPECTECX) impty batch

# The base + functions baseline (recursion builds on it); the recursion
# programs are the new work, so test-base leaves them out.
test-base:
	$(SPECTECX) impty batch --batch-dir tests/base
	$(SPECTECX) impty batch --batch-dir tests/functions
