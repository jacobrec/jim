#!/bin/bash

if [ "$1" = "test" ]; then
    echo "" | sbcl --load test_runner.lisp
elif [ "$1" = "cache" ]; then
    rm -rf ~/.cache/common-lisp/
    ./run.sh "${@:2}"
else
    sbcl --script appLaunch.lisp "$@" 2> crash;
    if [ $? -ne 0 ]; then
        reset
        tput rmcup
        echo -en "\e[?25h"
        cat crash
        echo "Terminal fixed after crash"
    fi
fi
