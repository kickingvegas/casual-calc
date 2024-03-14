##
# Copyright 2024 Charles Y. Choi
#

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")

EMACS_MAC_APP_HOME=/Applications/MacPorts/EmacsMac.app
EMACS_MAC_APP_SH=$(EMACS_MAC_APP_HOME)/Contents/MacOS/Emacs.sh

ifneq ("$(wildcard $(EMACS_MAC_APP_SH))","")
  EXEC_NAME=$(EMACS_MAC_APP_SH)
else
  EXEC_NAME=emacs
endif

ERT_ARGS=-batch -l ert
EMACS=$(EXEC_NAME) $(ERT_ARGS)
ERT_RUN=-f ert-run-tests-batch-and-exit
#ERT_RUN=-f ert-run-tests-batch

.PHONY: tests test-casual test-casual-graphics

# Note ordering of packages matters! Dependencies first.
tests: test-casual test-casual-graphics

test-casual:
	$(EMACS) -l ../casual-graphics.el \
-l ../casual.el \
-l casual-test-utils.el \
-l test-casual.el $(ERT_RUN)

test-casual-graphics:
	$(EMACS) -l ../casual-graphics.el \
-l casual-test-utils.el \
-l test-casual-graphics.el $(ERT_RUN)
