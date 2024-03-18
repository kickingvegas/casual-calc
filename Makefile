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

## Requirements
# - Python 3.11+
# - GNU awk 5.3+
# - Python semver
# - Bash

## User Actions
# To run test suite:
#   $ make tests

# To update development branch with fetch and pull
#   $ make checkout-development

# To create a pull request on the development branch:
#   $ make create-pr

# Bumping development version after PR is merged:
#   $ make BUMP_LEVEL=prerelease bump

# To create a pull request on the main branch:
#   $ make create-release-pr

# To create a new release tag (only after merging create-release-pr):
#   $ make BUMP_LEVEL={patch|minor|major} create-release-tag

# To setup for new sprint development:
#   $ make new-sprint

CASUAL_LISP_DIR=./lisp
CASUAL_EL=$(CASUAL_LISP_DIR)/casual.el

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
CASUAL_VERSION := $(shell ./scripts/read-version.sh $(CASUAL_EL))
# BUMP_LEVEL: major|minor|patch|prerelease|build
BUMP_LEVEL=patch
CASUAL_VERSION_BUMP := $(shell python -m semver bump $(BUMP_LEVEL) $(CASUAL_VERSION))
CASUAL_VERSION_PRERELEASE := $(shell python -m semver nextver $(CASUAL_VERSION) prerelease)

.PHONY: tests					\
create-pr					\
bump-casual					\
bump						\
checkout-development				\
checkout-main					\
sync-development-with-main			\
new-sprint					\
create-merge-development-branch			\
create-pr					\
create-release-pr				\
create-release-tag				\
create-gh-release

## Run test regression
tests:
	$(MAKE) -C tests tests

## Bump Patch Version
bump-casual:
	sed -i 's/;; Version: $(CASUAL_VERSION)/;; Version: $(CASUAL_VERSION_BUMP)/' $(CASUAL_EL)
	sed -i 's/(defconst casual-version "$(CASUAL_VERSION)"/(defconst casual-version "$(CASUAL_VERSION_BUMP)"/' $(CASUAL_EL)

bump: checkout-development bump-casual
	git commit -m 'Bump version to $(CASUAL_VERSION_BUMP)' $(CASUAL_EL)
	git push

checkout-development:
	git checkout development
	git branch --set-upstream-to=origin/development development
	git fetch origin --prune
	git pull

checkout-main:
	git checkout main
	git branch --set-upstream-to=origin/main main
	git fetch origin --prune
	git pull

sync-development-with-main: checkout-main checkout-development
	git merge main

new-sprint: sync-development-with-main
	sed -i 's/;; Version: $(CASUAL_VERSION)/;; Version: $(CASUAL_VERSION_PRERELEASE)/' $(CASUAL_EL)
	sed -i 's/(defconst casual-version "$(CASUAL_VERSION)"/(defconst casual-version "$(CASUAL_VERSION_PRERELEASE)"/' $(CASUAL_EL)
	git commit -m 'Bump version to $(CASUAL_VERSION_PRERELEASE)' $(CASUAL_EL)
	git push

create-merge-development-branch: checkout-development
	git checkout -b merge-development-to-main-$(TIMESTAMP)
	git push --set-upstream origin merge-development-to-main-$(TIMESTAMP)

## Create GitHub pull request for development
create-pr:
	gh pr create --base development --fill

## Create GitHub pull request for release
create-release-pr: create-merge-development-branch bump
	gh pr create --base main \
--title "Merge development to main $(TIMESTAMP)" \
--fill-verbose

create-release-tag: checkout-main
	git tag $(CASUAL_VERSION)
	git push origin $(CASUAL_VERSION)

create-gh-release:
	gh release create -t v$(CASUAL_VERSION) --notes-from-tag $(CASUAL_VERSION)
