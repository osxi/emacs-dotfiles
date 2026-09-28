#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd -P)"

emacs --batch --init-directory "$REPO_DIR" \
  --eval '(setq debug-on-error t)' \
  -l "$REPO_DIR/init.el"

emacs --batch \
  -l ert \
  -l "$REPO_DIR/lisp/init-functions.el" \
  -l "$REPO_DIR/lisp/keybindings.el" \
  -l "$REPO_DIR/test/init-functions-test.el" \
  -l "$REPO_DIR/test/keybindings-test.el" \
  -f ert-run-tests-batch-and-exit
