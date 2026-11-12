#!/usr/bin/env bash
# Run godel's ERT tests in a clean batch Emacs against Doom's built packages.
here=$(cd "$(dirname "$0")" && pwd)
build=${GODEL_BUILD_DIR:-$HOME/.config/emacs/.local/straight/build-30.2}
lp=()
for p in "$build"/*/; do lp+=(-L "$p"); done
exec emacs -Q --batch "${lp[@]}" -L "$here/.." -L "$here" -l godel-test -f ert-run-tests-batch-and-exit
