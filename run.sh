#!/bin/bash

if [ "$1" = "test" ]; then
    echo "" | sbcl --load test_runner.lisp
else
    sbcl --script appLaunch.lisp 2> crash;
    reset
    tput rmcup
    echo -en "\e[?25h"
    cat crash
fi
