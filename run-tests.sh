#!/usr/bin/env bash
# Run ERT tests for this config

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")" && pwd -P)"

emacs --batch \
  -l ert \
  -l "$REPO_DIR/lisp/init-functions.el" \
  -l "$REPO_DIR/lisp/keybindings.el" \
  -l "$REPO_DIR/test/init-functions-test.el" \
  -l "$REPO_DIR/test/keybindings-test.el" \
  -f ert-run-tests-batch-and-exit
