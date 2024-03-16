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
-l ../casual-predicates.el \
-l ../casual-labels.el \
-l ../casual-radix.el \
-l ../casual-angle-measure.el

CASUAL_PACKAGES=\
-l ../casual-binary.el				\
-l ../casual-complex.el				\
-l ../casual-conversion.el			\
-l ../casual-logarithmic.el			\
-l ../casual-random.el				\
-l ../casual-rounding.el			\
-l ../casual-rounding.el			\
-l ../casual-settings.el			\
-l ../casual-time.el				\
-l ../casual-trigonometric.el			\
-l ../casual-units.el				\
-l ../casual-vector.el				\
-l ../casual-graphics.el

CASUAL_TEST_INCLUDES=-l casual-test-utils.el

PACKAGE_TEST_COMMAND=$(EMACS) \
$(CASUAL_INCLUDES) \
-l ../$@.el \
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
-l ../$@.el \
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
