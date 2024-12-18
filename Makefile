test:
	emacs -q -Q -batch -l ert -l ./xmltokf.el -l ./tests.el \
	-f ert-run-tests-batch-and-exit
