#!/usr/bin/env bash
# Run notdiamond claude against godel-probe using a temporary config copy.
# Usage: probe.sh OUT.json [claude args...]
set -euo pipefail
out=$1; shift
here=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
python3 -I - "$HOME/.notdiamond/config.json" "$tmp/config.json" "$here/godel-probe" <<'PY'
import json, sys
src, dst, probe = sys.argv[1:]
cfg = json.load(open(src))
cfg["claudeBinaryPath"] = probe
json.dump(cfg, open(dst, "w"), indent=2)
PY
start=$(date +%s.%N)
GODEL_PROBE_OUT=$out NOTDIAMOND_CONFIG=$tmp/config.json \
  timeout 30 "$HOME/.npm-global/bin/notdiamond" claude "$@" </dev/null >"$tmp/stdout" 2>"$tmp/stderr" || echo "exit: $?"
echo "elapsed: $(echo "$(date +%s.%N) - $start" | bc)s"
echo "--- wrapper stderr"; cat "$tmp/stderr"
echo "--- wrapper stdout"; head -20 "$tmp/stdout"
