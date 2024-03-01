##
# Copyright 2024 Charles Y. Choi
#

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
INSTALL_DIR=$(HOME)/bin
EXEC_NAME=/Applications/MacPorts/EmacsMac.app/Contents/MacOS/Emacs.sh
ERT_ARGS=-batch -l ert
EMACS=$(EXEC_NAME) $(ERT_ARGS)
ERT_RUN=-f ert-run-tests-batch-and-exit
#ERT_RUN=-f ert-run-tests-batch

.PHONY: tests

tests:
	$(EMACS) -l ../casual.el \
-l test-casual.el $(ERT_RUN)
