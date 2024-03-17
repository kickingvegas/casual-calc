##
# Copyright 2024 Charles Y. Choi
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

.PHONY: tests \
casual-angle-measure				\
casual-binary					\
casual-complex					\
casual-conversion				\
casual-graphics					\
casual-labels					\
casual-logarithmic				\
casual-predicates				\
casual-radix					\
casual-random					\
casual-rounding					\
casual-settings					\
casual-time					\
casual-trigonometric				\
casual-units					\
casual-vector					\
casual

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
CASUAL_LISP_DIR=../lisp

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

CASUAL_INCLUDES=\
-l $(CASUAL_LISP_DIR)/casual-predicates.el	\
-l $(CASUAL_LISP_DIR)/casual-labels.el		\
-l $(CASUAL_LISP_DIR)/casual-radix.el		\
-l $(CASUAL_LISP_DIR)/casual-angle-measure.el

CASUAL_PACKAGES=				\
-l $(CASUAL_LISP_DIR)/casual-binary.el		\
-l $(CASUAL_LISP_DIR)/casual-complex.el		\
-l $(CASUAL_LISP_DIR)/casual-conversion.el	\
-l $(CASUAL_LISP_DIR)/casual-logarithmic.el	\
-l $(CASUAL_LISP_DIR)/casual-random.el		\
-l $(CASUAL_LISP_DIR)/casual-rounding.el	\
-l $(CASUAL_LISP_DIR)/casual-rounding.el	\
-l $(CASUAL_LISP_DIR)/casual-settings.el	\
-l $(CASUAL_LISP_DIR)/casual-time.el		\
-l $(CASUAL_LISP_DIR)/casual-trigonometric.el	\
-l $(CASUAL_LISP_DIR)/casual-units.el		\
-l $(CASUAL_LISP_DIR)/casual-vector.el		\
-l $(CASUAL_LISP_DIR)/casual-graphics.el

CASUAL_TEST_INCLUDES=-l casual-test-utils.el

PACKAGE_TEST_COMMAND=$(EMACS) \
$(CASUAL_INCLUDES) \
-l $(CASUAL_LISP_DIR)/$@.el \
$(CASUAL_TEST_INCLUDES) \
-l test-$@.el $(ERT_RUN)

tests: \
casual-angle-measure				\
casual-binary					\
casual-complex					\
casual-conversion				\
casual-graphics					\
casual-labels					\
casual-logarithmic				\
casual-predicates				\
casual-radix					\
casual-random					\
casual-rounding					\
casual-settings					\
casual-time					\
casual-trigonometric				\
casual-units					\
casual-vector					\
casual

casual:
	$(EMACS) \
$(CASUAL_INCLUDES) \
$(CASUAL_PACKAGES) \
-l $(CASUAL_LISP_DIR)/$@.el \
$(CASUAL_TEST_INCLUDES) \
-l test-$@.el $(ERT_RUN)

casual-angle-measure:
	$(PACKAGE_TEST_COMMAND)
casual-binary:
	$(PACKAGE_TEST_COMMAND)
casual-complex:
	$(PACKAGE_TEST_COMMAND)
casual-conversion:
	$(PACKAGE_TEST_COMMAND)
casual-graphics:
	$(PACKAGE_TEST_COMMAND)
casual-labels:
	$(PACKAGE_TEST_COMMAND)
casual-logarithmic:
	$(PACKAGE_TEST_COMMAND)
casual-predicates:
	$(PACKAGE_TEST_COMMAND)
casual-radix:
	$(PACKAGE_TEST_COMMAND)
casual-random:
	$(PACKAGE_TEST_COMMAND)
casual-rounding:
	$(PACKAGE_TEST_COMMAND)
casual-settings:
	$(PACKAGE_TEST_COMMAND)
casual-time:
	$(PACKAGE_TEST_COMMAND)
casual-trigonometric:
	$(PACKAGE_TEST_COMMAND)
casual-units:
	$(PACKAGE_TEST_COMMAND)
casual-vector:
	$(PACKAGE_TEST_COMMAND)
