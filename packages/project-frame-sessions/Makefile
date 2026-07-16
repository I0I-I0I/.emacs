EMACS ?= emacs
.PHONY: test test-unit test-concurrency test-graphical compile checkdoc package-lint quality

test: test-unit test-concurrency

test-unit:
	rm -f project-frame-sessions.elc
	$(EMACS) -Q --batch -L . -l project-frame-sessions-tests.el -f ert-run-tests-batch-and-exit

test-concurrency:
	rm -f project-frame-sessions.elc
	$(EMACS) -Q --batch -L . -l project-frame-sessions-concurrency-tests.el -f ert-run-tests-batch-and-exit

test-graphical:
	@test -n "$$DISPLAY" || { echo "DISPLAY is required for graphical tests"; exit 1; }
	$(EMACS) -Q --display "$$DISPLAY" -L . -l project-frame-sessions-graphical-tests.el --eval '(let ((stats (ert-run-tests-batch "^project-frame-sessions-graphical-"))) (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))'

compile:
	rm -f project-frame-sessions.elc
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile project-frame-sessions.el

checkdoc:
	$(EMACS) -Q --batch -L . --eval '(progn (require (quote checkdoc)) (checkdoc-file "project-frame-sessions.el"))'

package-lint:
	$(EMACS) -Q --batch -L . --eval '(if (require (quote package-lint) nil t) (package-lint-batch-and-exit) (message "package-lint unavailable; skipped"))' project-frame-sessions.el

quality: compile checkdoc package-lint
