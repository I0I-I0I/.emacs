EMACS ?= emacs
.PHONY: test test-unit test-concurrency test-graphical compile checkdoc package-lint quality

test: test-unit test-concurrency

test-unit:
	$(EMACS) -Q --batch -L . -l project-frame-sessions-tests.el -f ert-run-tests-batch-and-exit

test-concurrency:
	$(EMACS) -Q --batch -L . -l project-frame-sessions-concurrency-tests.el -f ert-run-tests-batch-and-exit

test-graphical:
	$(EMACS) -Q --batch -L . -l project-frame-sessions-graphical-tests.el -f ert-run-tests-batch-and-exit

compile:
	rm -f project-frame-sessions.elc
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile project-frame-sessions.el

checkdoc:
	$(EMACS) -Q --batch -L . --eval '(progn (require (quote checkdoc)) (checkdoc-file "project-frame-sessions.el"))'

package-lint:
	$(EMACS) -Q --batch -L . --eval '(if (require (quote package-lint) nil t) (package-lint-batch-and-exit) (message "package-lint unavailable; skipped"))' project-frame-sessions.el

quality: compile checkdoc package-lint
