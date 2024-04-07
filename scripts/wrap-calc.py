#!/usr/bin/env python3
#
# Copyright 2024 Yummy Melon Software

import os
import sys
import argparse
from subprocess import Popen, PIPE, call
import shutil
from datetime import datetime
from string import Template

VERSION = "1.0.0"

class CommandLineParser:
    def __init__(self,
                 prog="wrap-calc",
                 description="wrap calc functions",
                 epilog="this is done",
                 version=VERSION):
        self.parser = argparse.ArgumentParser(
            prog=prog,
            description=description,
            epilog=epilog
        )

        self.parser.add_argument('-o', '--output',
                                 action='store',
                                 default='-',
                                 help='output file (- for stdout)')

        self.parser.add_argument('-i', '--info',
                                 action='store',
                                 help='info page')

        self.parser.add_argument('-t', '--testfile',
                                 action='store',
                                 default='tests.el',
                                 help='test file')

        self.parser.add_argument('-v', '--version',
                                 action='version',
                                 version=version,
                                 help='print version information and exit')

        self.parser.add_argument('target',
                                 action='store',
                                 help='target file')

    def run(self):
        return self.parser.parse_args()

class Application:
    def __init__(self, args):
        self.version = VERSION
        self.stdout = sys.stdout
        self.stdin = sys.stdin
        self.stderr = sys.stderr

        self.args = args

        if args.output != '-':
            outfile = open('{0}'.format(args.output), 'w')
            self.stdout = outfile

        self.wrappedCalcFunctionTemplate = Template(
"""(defun casual-$fn ()
    "TODO: This function does not yet have a docstring.
\\nStack Arguments:
1: n

This function is a wrapper over `$fn'.

* References
- info node `(calc) $info'
- `$fn'"
 (interactive)
 (call-interactively #'$fn))"""
        )

        self.wrappedCalcFunctionTestTemplate = Template(
            """;; (ert-deftest test-casual-$fn ()
;;   (casualt-setup)
;;   (calc-push 10)
;;   (casualt-testbench-calc-fn #'casual-$fn
;;                              '()
;;                              '(float 647943975411 -8))
;;   (casualt-breakdown t))"""
        )

    def run(self):
        d = dict(fn=self.args.target,
                 info=self.args.info)

        wrappedFunction = self.wrappedCalcFunctionTemplate.substitute(d)
        self.stdout.write(wrappedFunction)
        self.stdout.write("\n\n")

        with open(self.args.testfile, "a+") as testFile:
            testFile.write(
                self.wrappedCalcFunctionTestTemplate.substitute(d)
            )
            testFile.write("\n\n")


        # wrap up
        if self.stdout != sys.stdout:
            self.stdout.close()

if __name__ == '__main__':
    app = Application(CommandLineParser().run())
    app.run()
