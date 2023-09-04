#!/bin/bash

d=$(cd $(dirname $0); pwd)

run_emacs() {
    emacs --batch --quick --directory . "$@"
}

set -ex

run_emacs --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" *.el
run_emacs --load tests/*.el --eval "(ert-run-tests-batch-and-exit t)"
