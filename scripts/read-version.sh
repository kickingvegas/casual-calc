#!/usr/bin/env bash
##
# Copyright 2024 Charles Y. Choi
#

grep 'Version: ' $1 | awk '{print $3}'
