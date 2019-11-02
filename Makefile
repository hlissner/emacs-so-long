# -*- indent-tabs-mode: t; -*-
EMACS ?= emacs

TESTSUITES = so-long-tests.elc \
	autoload-longlines-mode-tests.elc \
	autoload-minor-mode-tests.elc \
	autoload-major-mode-tests.elc \
	spelling-tests.elc

all: compile test

clean:
	rm -f so-long.elc tests/*.elc

compile:
	$(EMACS) -batch -L . -f batch-byte-compile so-long.el tests/*.el

test:
	for suite in $(TESTSUITES); do \
		$(EMACS) -batch -L . -l tests/$$suite -f ert-run-tests-batch-and-exit; \
	done

.PHONY: all clean compile test
