#!/bin/bash

if [ "$1" = "test" ]; then
    echo "" | sbcl --load test_runner.lisp
else
    ./appLaunch.lisp 2> crash; reset; tput rmcup; cat crash
fi
