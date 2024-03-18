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

CASUAL_INCLUDES =				\
casual-predicates.el				\
casual-labels.el				\
casual-radix.el					\
casual-angle-measure.el

CASUAL_PACKAGES=				\
casual-binary.el				\
casual-complex.el				\
casual-conversion.el				\
casual-logarithmic.el				\
casual-random.el				\
casual-rounding.el				\
casual-rounding.el				\
casual-settings.el				\
casual-stack.el					\
casual-time.el					\
casual-trail.el					\
casual-trigonometric.el				\
casual-units.el					\
casual-vector.el				\
casual-graphics.el

CASUAL_TEST_INCLUDES= \
casual-test-utils.el

.PHONY: tests compile regression

.SUFFIXES: .el .elc .elt

.el.elc :
	$(EXEC_NAME) -Q --batch $(patsubst %, -l %, $(CASUAL_INCLUDES)) \
-f batch-byte-compile $<

.el.elt :
	$(EXEC_NAME) -Q --batch $(patsubst %, -l %, $(CASUAL_INCLUDES)) \
-l $< \
-l $(patsubst %, ../tests/%, $(CASUAL_TEST_INCLUDES)) \
-l $(patsubst %, ../tests/test-%, $<) \
-f ert-run-tests-batch-and-exit

tests: $(CASUAL_PACKAGES:.el=.elt) casual.elt

compile: $(CASUAL_PACKAGES:.el=.elc) casual.elc

casual.elc: casual.el
	$(EXEC_NAME) -Q --batch $(patsubst %, -l %, $(CASUAL_INCLUDES)) \
$(patsubst %, -l %, $(CASUAL_PACKAGES)) \
-f batch-byte-compile $<

casual.elt: casual.el
	$(EXEC_NAME) -Q --batch \
$(patsubst %, -l %, $(CASUAL_INCLUDES)) \
$(patsubst %, -l %, $(CASUAL_PACKAGES)) \
-l $< \
-l ../tests/casual-test-utils.el \
-l $(patsubst %, ../tests/test-%, $<) \
-f ert-run-tests-batch-and-exit

regression: clean compile tests

clean:
	rm -f *.elc
